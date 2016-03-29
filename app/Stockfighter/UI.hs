
{-# LANGUAGE RecordWildCards #-}

module Stockfighter.UI
  ( startBlotter
  ) where

import           Brick
import           Data.Default (def)
import           Data.HashMap.Strict (HashMap)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Stockfighter
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
        -- TODO: account starting balance
        let bBalance = determineBalance . IM.elems <$> bOrders


        ---- Widgets

        -- -- "Standing" widget
        -- bStandingWidget :: Behavior Widget
        let bStandingWidget = makeStanding <$> bBalance <*> bOwned

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
                           , eExit = never
                           }


insertOrder :: UserOrder -> IntMap UserOrder -> IntMap UserOrder
insertOrder order = IM.insert (uoOrderId order) order

determineOwned :: [UserOrder] -> HashMap Symbol Int
determineOwned = undefined

determineBalance :: [UserOrder] -> Int
determineBalance = undefined


makeStanding :: Int -- Balance
             -> HashMap Symbol Int -- Owned stock
             -> Widget
makeStanding = undefined

makeBidsAsks :: [UserOrder] -> Widget
makeBidsAsks = undefined

makeTickers :: Event Quote -> MomentIO (Behavior Widget)
makeTickers = undefined

makeInterface :: Widget -- Standing widget
              -> Widget -- Bids/asks
              -> Widget -- Tickers/graphs
              -> Widget
makeInterface = undefined
