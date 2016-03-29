{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Stockfighter.Client
  ( StockfighterT
  , runStockfighter
  , APIException(..)
  , SfInst(..)

  -- GM calls
  , withLevel
  , startLevel
  , stopLevel
  , resumeLevel
  , restartLevel
  , instInfo
  -- , listLevels -- TODO: not yet supported by stockfighter

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
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Network.Wreq
import Stockfighter.Client.Internal
import Stockfighter.Types


---- GM calls

type StockfighterT = ReaderT ApiKey

runStockfighter :: ApiKey -> StockfighterT m a -> m a
runStockfighter = flip runReaderT


withLevel :: (MonadAPI m, MonadMask m) => String -> (SfLevel -> m a) -> m a
withLevel name f = do
    level <- startLevel name
    f level `finally` stopLevel (instanceId level)


emptyPost :: [FormParam]
emptyPost = []

startLevel :: MonadAPI m => String -> m SfLevel
startLevel level = postGM ["levels", level] emptyPost

stopLevel :: MonadAPI m => InstanceId -> m Bool
stopLevel (InstanceId inst) = respOk <$> postGM ["instances", show inst, "stop"] emptyPost

resumeLevel :: MonadAPI m => InstanceId -> m Bool
resumeLevel (InstanceId inst) = respOk <$> postGM ["instances", show inst, "resume"] emptyPost

restartLevel :: MonadAPI m => InstanceId -> m Bool
restartLevel (InstanceId inst) = respOk <$> postGM ["instances", show inst, "restart"] emptyPost

instInfo :: MonadAPI m => InstanceId -> m SfInst
instInfo inst = getGM ["instances", show (unInstanceId inst)]


---- GET

heartbeat :: MonadAPI m => m Bool
heartbeat = respOk <$> getAPI ["heartbeat"]

venueHeartbeat :: MonadAPI m => Venue -> m Bool
venueHeartbeat (Venue venue) = respOk <$> getAPI ["venues", venue, "heartbeat"]

stocks :: MonadAPI m => Venue -> m [Stock]
stocks (Venue venue) =
    getAPIWith ["venues", venue, "stocks"] (^? key "symbols")

orderbook :: MonadAPI m => Venue -> Symbol -> m OrderBook
orderbook (Venue venue) (Symbol symbol) =
    getAPI ["venues", venue, "stocks", symbol]

quote :: MonadAPI m => Venue -> Symbol -> m Quote
quote (Venue venue) (Symbol symbol) =
    getAPI ["venues", venue, "stocks", symbol, "quote"]

orderStatus :: MonadAPI m => Venue -> Symbol -> Int -> m UserOrder
orderStatus (Venue venue) (Symbol symbol) orderId =
    getAPI ["venues", venue, "stocks", symbol, "orders", show orderId]

allOrders :: MonadAPI m => Account -> Venue -> m [UserOrder]
allOrders (Account account) (Venue venue) =
    getAPI ["venues", venue, "accounts", account, "orders"]

ordersForStock :: MonadAPI m => Account -> Venue -> Symbol -> m [UserOrder]
ordersForStock (Account account) (Venue venue) (Symbol symbol) =
    getAPIWith ["venues", venue, "accounts", account, "stocks", symbol, "orders"]
               (^? key "orders")


---- POST

placeOrder :: MonadAPI m
           => Account
           -> Venue
           -> Symbol
           -> Int -- Price
           -> Int -- Quantity
           -> Direction
           -> OrderType
           -> m UserOrder
placeOrder (Account account) (Venue venue) (Symbol symbol) price quantity direction orderType =
    postAPI ["venues", venue, "stocks", symbol, "orders"]
                    (object [ "account"   .= account
                            , "venue"     .= venue
                            , "stock"     .= symbol
                            , "price"     .= price
                            , "qty"       .= quantity
                            , "direction" .=
                                  case direction of
                                      Ask -> "sell" :: String
                                      Bid -> "buy"
                            , "orderType" .=
                                  case orderType of
                                      Limit -> "limit" :: String
                                      Market -> "market"
                                      FillOrKill -> "fill-or-kill"
                                      ImmediateOrCancel -> "immediate-or-cancel"
                            ])


---- DELETE

cancelOrder :: MonadAPI m
            => Venue
            -> Symbol
            -> Int    -- Order ID
            -> m UserOrder
cancelOrder (Venue venue) (Symbol symbol) orderId =
    deleteAPI ["venues", venue, "stocks", symbol, "orders", show orderId]


---- Websockets

tickerTape :: MonadIO m => Account -> Venue -> (Quote -> IO ()) -> m ThreadId
tickerTape (Account account) (Venue venue) =
    tapeWith [account, "venues", venue, "tickertape"]
             (^? key "quote")

tickerTapeStock :: MonadIO m => Account -> Venue -> Symbol -> (Quote -> IO ()) -> m ThreadId
tickerTapeStock (Account account) (Venue venue) (Symbol symbol) =
    tapeWith [account, "venues", venue, "tickertape", "stocks", symbol]
             (^? key "quote")

executions :: MonadIO m => Account -> Venue -> (Execution -> IO ()) -> m ThreadId
executions (Account account) (Venue venue) =
    tape [account, "venues", venue, "executions"]

executionsStock :: MonadIO m => Account -> Venue -> Symbol -> (Execution -> IO ()) -> m ThreadId
executionsStock (Account account) (Venue venue) (Symbol symbol) =
    tape [account, "venues", venue, "executions", "stocks", symbol]
