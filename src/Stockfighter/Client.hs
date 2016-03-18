{-# LANGUAGE OverloadedStrings #-}

module Stockfighter.Client
  ( APIError(..)
  , SfOpts(..)
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

venueHeartbeat :: MonadIO m => StockfighterT m Bool
venueHeartbeat = respOk <$> getVenue "heartbeat"

stocks :: MonadIO m => StockfighterT m [Stock]
stocks = getVenueWith "stocks" (^? key "symbols")

orderbook :: MonadIO m => String -> StockfighterT m OrderBook
orderbook stock = getVenue ("stocks/" ++ stock)

quote :: MonadIO m => String -> StockfighterT m Quote
quote stock = getVenue ("stocks/" ++ stock ++ "/quote")

orderStatus :: MonadIO m => String -> Int -> StockfighterT m UserOrder
orderStatus stock orderId = getVenue ("stocks/" ++ stock ++ "/orders/" ++ show orderId)

allOrders :: MonadIO m => StockfighterT m [UserOrder]
allOrders = do
    _account <- asks optAccount
    getVenue ("accounts/" ++ _account ++ "/orders")

ordersForStock :: MonadIO m => String -> StockfighterT m [UserOrder]
ordersForStock stock = do
    _account <- asks optAccount
    getVenueWith ("accounts/" ++ _account ++ "/stocks/" ++ stock ++ "/orders") (^? key "orders")


---- POST

placeOrder :: MonadIO m
           => String -- Stock symbol
           -> Int    -- Price
           -> Int    -- Quantity
           -> Direction
           -> OrderType
           -> StockfighterT m UserOrder
placeOrder stock _price quantity _direction _orderType = do
    _account <- asks optAccount
    _venue   <- asks optVenue
    postVenue ("stocks/" ++ stock ++ "/orders")
              (object [ "account"   .= _account
                      , "venue"     .= _venue
                      , "stock"     .= stock
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
            => String -- Stock symbol
            -> Int    -- Order ID
            -> StockfighterT m UserOrder
cancelOrder stock orderId = deleteVenue ("stocks/" ++ stock ++ "/orders/" ++ show orderId)


---- Websockets

tickerTape :: MonadIO m => StockfighterT m (ThreadId, TMChan Quote)
tickerTape = do
    _account <- asks optAccount
    _venue   <- asks optVenue
    tapeWith (_account ++ "/venues/" ++ _venue ++ "/tickertape") (^? key "quote")

tickerTapeStock :: MonadIO m => String -> StockfighterT m (ThreadId, TMChan Quote)
tickerTapeStock stock = do
    _account <- asks optAccount
    _venue   <- asks optVenue
    tapeWith (_account ++ "/venues/" ++ _venue ++ "/tickertape/stocks/" ++ stock) (^? key "quote")

executions :: MonadIO m => StockfighterT m (ThreadId, TMChan Execution)
executions = do
    _account <- asks optAccount
    _venue   <- asks optVenue
    tape (_account ++ "/venues/" ++ _venue ++ "/executions")

executionsStock :: MonadIO m => String -> StockfighterT m (ThreadId, TMChan Execution)
executionsStock stock = do
    _account <- asks optAccount
    _venue   <- asks optVenue
    tape (_account ++ "/venues/" ++ _venue ++ "/executions/stocks/" ++ stock)
