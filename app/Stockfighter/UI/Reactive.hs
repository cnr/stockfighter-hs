{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Stockfighter.UI.Reactive
  ( reactiveMain

  , ReactiveBuilder
  , ReactiveApp(..)
  , RenderView(..)

  , VtyEvents(..)
  , VKey(..)
  , VMouse(..)
  , VResize(..)
  ) where

import           Brick
import           Brick.Widgets.Border.Style
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Default
import qualified Graphics.Vty as V
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Unsafe.Coerce

type ReactiveBuilder a = VtyEvents -> MomentIO (ReactiveApp a)

data VtyEvents = VtyEvents { eKey    :: Event VKey
                           , eMouse  :: Event VMouse
                           , eResize :: Event VResize
                           }

data VKey    = VKey    V.Key [V.Modifier]
data VMouse  = VMouse  Int   Int          V.Button [V.Modifier]
data VResize = VResize Int   Int

data ReactiveApp a = ReactiveApp { bView :: Behavior RenderView
                                 , eExit :: Event a
                                 }

data RenderView = RenderView { rWidgets :: [Widget]
                             , rCursor  :: Maybe CursorLocation
                             , rAttrMap :: AttrMap
                             }

reactiveMain :: ReactiveBuilder a -> IO a
reactiveMain app = withVty $ \vty -> do
    (esVtyEvents, fireVtyEvent) <- newAddHandler

    exitVar <- newEmptyMVar

    network <- compile $ do
        (eKey   , fireKey)    <- newEvent
        (eMouse , fireMouse)  <- newEvent
        (eResize, fireResize) <- newEvent

        _ <- liftIO $ register esVtyEvents (\case
                V.EvKey k m       -> fireKey    (VKey k m)
                V.EvMouse x y b m -> fireMouse  (VMouse x y b m)
                V.EvResize w h    -> fireResize (VResize w h))

        ReactiveApp{..} <- app VtyEvents{..}

        -- Do an initial render
        liftIO . renderView vty =<< valueB bView

        -- Put our exit value
        reactimate (putMVar exitVar <$> eExit)
        -- Render each time our RenderView changes
        reactimate' . fmap (fmap (renderView vty)) =<< changes bView

    actuate network
    vtySupplyId <- forkIO $ forever (fireVtyEvent =<< V.nextEvent vty)

    takeMVar exitVar `finally` (pause network >> killThread vtySupplyId)

withVty :: (V.Vty -> IO a) -> IO a
withVty f = do
    vty <- V.mkVty def
    f vty `finally` V.shutdown vty

renderView :: V.Vty -> RenderView -> IO ()
renderView vty RenderView{..} = do
    (width,height) <- V.displayBounds (V.outputIface vty)

    let ctx    = toContext (RContext def width height def rAttrMap)
        rstate = error "Viewports are not yet supported"

        layers = flip evalState rstate
               . traverse (`runReaderT` ctx)
               . map (render . cropToContext)
               $ rWidgets

        pic    = V.picForLayers (V.resize width height . image <$> layers)

        cursor = maybe V.NoCursor (uncurry V.Cursor . loc . cursorLocation) rCursor

    V.update vty pic { V.picCursor = cursor}


-- Brick doesn't export Context
toContext :: RContext -> Context
toContext = unsafeCoerce

data RContext = RContext AttrName    -- ctxAttrName
                         Int         -- availWidth
                         Int         -- availHeight
                         BorderStyle -- ctxBorderStyle
                         AttrMap     -- ctxAttrMap
