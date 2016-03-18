{-# LANGUAGE OverloadedStrings #-}

module Stockfighter.Client
  ( APIError(..)
  , SfLevel(..)
  , StockfighterT
  , runStockfighter

  -- GET
  , allOrders
  , heartbeat
  , orderbook
  , ordersForStock
  , orderStatus
  , quote
  , stocks
  , venueHeartbeat

  -- POST
  , placeOrder

  -- DELETE
  , cancelOrder

  -- WebSockets
  , tickerTape
  , tickerTapeStock
  , executions
  , executionsStock
  ) where

import Control.Concurrent
import Control.Concurrent.STM.TMChan
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Stockfighter.Client.Internal
import Stockfighter.Types


---- GET

heartbeat :: MonadIO m => StockfighterT m Bool
heartbeat = respOk <$> get "heartbeat"

venueHeartbeat :: MonadIO m => Venue -> StockfighterT m Bool
venueHeartbeat venue = respOk <$> getVenue venue "heartbeat"

stocks :: MonadIO m => Venue -> StockfighterT m [Stock]
stocks venue = getVenueWith venue "stocks" (^? key "symbols")

orderbook :: MonadIO m => Venue -> Symbol -> StockfighterT m OrderBook
orderbook venue (Symbol sym) = getVenue venue ("stocks/" ++ sym)

quote :: MonadIO m => Venue -> Symbol -> StockfighterT m Quote
quote venue (Symbol sym) = getVenue venue ("stocks/" ++ sym ++ "/quote")

orderStatus :: MonadIO m => Venue -> Symbol -> Int -> StockfighterT m UserOrder
orderStatus venue (Symbol symbol) orderId = getVenue venue ("stocks/" ++ symbol ++ "/orders/" ++ show orderId)

allOrders :: MonadIO m => Venue -> StockfighterT m [UserOrder]
allOrders venue = do
    account <- asks levelAccount
    getVenue venue ("accounts/" ++ account ++ "/orders")

ordersForStock :: MonadIO m => Venue -> Symbol -> StockfighterT m [UserOrder]
ordersForStock venue (Symbol symbol) = do
    account <- asks levelAccount
    getVenueWith venue ("accounts/" ++ account ++ "/stocks/" ++ symbol ++ "/orders") (^? key "orders")


---- POST

placeOrder :: MonadIO m
           => Venue
           -> Symbol
           -> Int    -- Price
           -> Int    -- Quantity
           -> Direction
           -> OrderType
           -> StockfighterT m UserOrder
placeOrder venue symbol _price quantity _direction _orderType = do
    account <- asks levelAccount
    postVenue venue ("stocks/" ++ unSymbol symbol ++ "/orders")
                    (object [ "account"   .= account
                            , "venue"     .= unVenue venue
                            , "stock"     .= unSymbol symbol
                            , "price"     .= _price
                            , "qty"       .= quantity
                            , "direction" .=
                                  case _direction of
                                      Ask -> "sell" :: String
                                      Bid -> "buy"
                            , "orderType" .=
                                  case _orderType of
                                      Limit -> "limit" :: String
                                      Market -> "market"
                                      FillOrKill -> "fill-or-kill"
                                      ImmediateOrCancel -> "immediate-or-cancel"
                            ])


---- DELETE

cancelOrder :: MonadIO m
            => Venue
            -> Symbol
            -> Int    -- Order ID
            -> StockfighterT m UserOrder
cancelOrder venue symbol orderId = deleteVenue venue ("stocks/" ++ unSymbol symbol ++ "/orders/" ++ show orderId)


---- Websockets

tickerTape :: MonadIO m => Venue -> StockfighterT m (ThreadId, TMChan Quote)
tickerTape venue = do
    account <- asks levelAccount
    tapeWith (account ++ "/venues/" ++ unVenue venue ++ "/tickertape") (^? key "quote")

tickerTapeStock :: MonadIO m => Venue -> Symbol -> StockfighterT m (ThreadId, TMChan Quote)
tickerTapeStock venue symbol = do
    account <- asks levelAccount
    tapeWith (account ++ "/venues/" ++ unVenue venue ++ "/tickertape/stocks/" ++ unSymbol symbol) (^? key "quote")

executions :: MonadIO m => Venue -> StockfighterT m (ThreadId, TMChan Execution)
executions venue = do
    account <- asks levelAccount
    tape (account ++ "/venues/" ++ unVenue venue ++ "/executions")

executionsStock :: MonadIO m => Venue -> Symbol -> StockfighterT m (ThreadId, TMChan Execution)
executionsStock venue symbol = do
    account <- asks levelAccount
    tape (account ++ "/venues/" ++ unVenue venue ++ "/executions/stocks/" ++ unSymbol symbol)
