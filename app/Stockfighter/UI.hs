
{-# LANGUAGE RecordWildCards #-}

module Stockfighter.UI
  ( blotterBuilder
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Stockfighter
import Stockfighter.UI.Padding
import Stockfighter.UI.Reactive

blotterBuilder :: ApiKey -> String -> ReactiveBuilder ()
blotterBuilder key levelName VtyEvents{..} = runStockfighter key $ do
    -- FRP Inputs:
    -- - Placed orders --> ePlaced
    -- - Canceled orders --> eCanceled
    --
    -- FRP Inputs from stockfighter:
    -- - Executions (fills) --> eExecutions
    -- - Quotes --> eQuotes
    --
    -- Non-FRP inputs from stockfighter:
    -- - initial balance
    --
    -- Global orders behavior --> bOrders :: Behavior __ UserOrder(??)
    -- - ePlaced
    -- - eCanceled
    -- - eExecutions
    --
    -- Owned stock --> bOwned
    -- - bOrders
    --
    -- Player balance --> bBalance :: Behavior Int
    -- - bOrders
    -- - initial balance
    --
    -- "Standing" widget --> bStanding :: Behavior Widget
    -- - bBalance
    -- - bNAV
    -- - bOwned
    --
    -- "Bids/asks" widgets --> bBidsAsks :: Behavior Widget
    -- - bOrders
    --
    -- "Stock ticker/graph" widget --> bTicker :: Behavior Widget
    -- - eQuotes
    undefined
