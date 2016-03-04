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
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson.Lens
import           Stockfighter.Client.Internal
import           Stockfighter.Types


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
    acc <- asks optAccount
    getVenue ("accounts/" ++ acc ++ "/orders")

ordersForStock :: MonadIO m => String -> StockfighterT m [UserOrder]
ordersForStock stock = do
    acc <- asks optAccount
    getVenueWith ("accounts/" ++ acc ++ "/stocks/" ++ stock ++ "/orders") (^? key "orders")


---- POST

placeOrder :: MonadIO m
           => String -- Stock symbol
           -> Int    -- Price
           -> Int    -- Quantity
           -> Direction
           -> OrderType
           -> StockfighterT m UserOrder
placeOrder = undefined


---- DELETE

cancelOrder :: MonadIO m
            => String -- Stock symbol
            -> Int    -- Order ID
            -> StockfighterT m UserOrder
cancelOrder = undefined
