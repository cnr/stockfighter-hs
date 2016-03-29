
{-# LANGUAGE RecordWildCards #-}

module Stockfighter.UI
  ( startBlotter
  ) where

import           Brick               hiding (row)
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Data.Default        (def)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IM
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Stockfighter
import           Stockfighter.UI.Padding
import           Stockfighter.UI.Reactive


startBlotter :: MonadIO m => SfLevel -> StockfighterT m ()
startBlotter SfLevel{..} = do
    (esQuotes, fireQuote)         <- liftIO newAddHandler
    (esExecutions, fireExecution) <- liftIO newAddHandler

    tapeTids <- traverse (\venue -> tickerTape levelAccount venue fireQuote) venues
    execTids <- traverse (\venue -> executions levelAccount venue fireExecution) venues
    -- TODO: kill threads

    reactiveMain $ \VtyEvents{..} -> do
        -- -- FRP Inputs from stockfighter
        -- eQuotes     :: Event Quote     -- The level's ticker quotes
        -- eExecutions :: Event Execution -- The level's executions (fills)
        eQuotes     <- fromAddHandler esQuotes
        eExecutions <- fromAddHandler esExecutions

        -- -- Looparound FRP Inputs (TODO)
        -- ePlaced   :: Event UserOrder -- When the user places an order
        -- eCanceled :: Event UserOrder -- When the user cancels an order
        let ePlaced   = never :: Event UserOrder
            eCanceled = never :: Event UserOrder

        -- -- User Orders
        -- bOrders :: Behavior (IntMap UserOrder)
        bOrders <- accumB IM.empty
                 $ unions
                 [ insertOrder          <$> ePlaced
                 , insertOrder          <$> eCanceled
                 , insertOrder . eOrder <$> eExecutions
                 ]

        -- -- Owned stock
        -- bOwned :: Behavior (HashMap Symbol Int)
        let bOwned = determineOwned . IM.elems <$> bOrders

        -- -- Player balance
        -- bBalance :: Behavior Int
        let startingBalance = fromMaybe 0 (M.lookup "USD" =<< balances)
            bBalance        = (startingBalance +) . determineBalance . IM.elems <$> bOrders


        ---- Widgets

        -- -- "Standing" widget
        -- bStandingWidget :: Behavior Widget
        let bStandingWidget = makeStanding <$> bBalance <*> fmap HM.toList bOwned

        -- -- "Bids/asks" widgets
        -- bBidsAsks :: Behavior Widget
        let bBidsAsks = makeBidsAsks . IM.elems <$> bOrders

        -- -- "Stock tickers/graphs" widget
        -- bTickers :: Behavior Widget
        bTickers <- makeTickers eQuotes


        -- -- The complete, composed widget
        -- bAppWidget :: Behavior Widget
        let bAppWidget = makeInterface <$> bStandingWidget <*> bBidsAsks <*> bTickers

        -- Finally, the moment we've all been waiting for:
        return ReactiveApp { bView = RenderView <$> fmap pure bAppWidget
                                                <*> pure (const Nothing)
                                                <*> pure def
                           , eExit = () <$ eKey
                           }


insertOrder :: UserOrder -> IntMap UserOrder -> IntMap UserOrder
insertOrder order = IM.insert (uoOrderId order) order

determineOwned :: [UserOrder] -> HashMap Symbol Int
determineOwned = foldr go HM.empty
  where
  go :: UserOrder -> HashMap Symbol Int -> HashMap Symbol Int
  go UserOrder{..} =
      case uoDirection of
          Ask -> HM.insertWith (+) uoSymbol (negate uoTotalFilled)
          Bid -> HM.insertWith (+) uoSymbol uoTotalFilled

determineBalance :: [UserOrder] -> Int
determineBalance = foldr go 0
  where
  go :: UserOrder -> Int -> Int
  go UserOrder{..} acc =
      case uoDirection of
          Ask -> acc - uoPrice * uoTotalFilled
          Bid -> acc + uoPrice * uoTotalFilled

makeStanding :: Int -- Balance
             -> [(Symbol, Int)] -- Owned stock
             -> Widget
makeStanding balance owned = border $ reifyCol $
    colP [ rowP [ fullJustify ["Standing"]]
         , rowP [ leftJustify  ("Balance"    : map (unSymbol . fst) owned)
                , fullJustify  [" "]
                , rightJustify (show balance : map (show     . snd) owned)
                ]
         ]

makeBidsAsks :: [UserOrder] -> Widget
makeBidsAsks _ = border $ str "Bids/Asks Widget" -- TODO

makeTickers :: Event Quote -> MomentIO (Behavior Widget)
makeTickers _ = return (pure (border $ str "Tickers Widget")) -- TODO

makeInterface :: Widget -- Standing widget
              -> Widget -- Bids/asks
              -> Widget -- Tickers/graphs
              -> Widget
makeInterface standing bidsAsks tickers = withBorderStyle ascii $
    vBox [standing, bidsAsks, tickers] -- TODO
