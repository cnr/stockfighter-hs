{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Stockfighter.Types
  ( Venue(..)
  , Symbol(..)

  , Direction(..)
  , OrderType(..)

  , Order(..)
  , OrderBook(..)
  , UserOrder(..)
  , Fill(..)
  , Quote(..)
  , Execution(..)
  , Stock(..)
  ) where

import           Control.Applicative (empty)
import           Data.Aeson          ((.:), (.:?), FromJSON(parseJSON), withObject, withText)
import           Data.Bool           (bool)
import           Data.String
import qualified Data.Text as T
import           Data.Time.Clock     (UTCTime)


---- Orders

data Direction = Ask | Bid deriving Show

data OrderType = Limit
               | Market
               | FillOrKill
               | ImmediateOrCancel
                 deriving Show

data Order     = Order     { oDirection    :: Direction
                           , oPrice        :: Int
                           , oQty          :: Int
                           } deriving Show

data OrderBook = OrderBook { obVenue       :: Venue
                           , obSymbol      :: Symbol
                           , obBidList     :: [Order]
                           , obAskList     :: [Order]
                           } deriving Show

data UserOrder = UserOrder { uoVenue       :: Venue
                           , uoSymbol      :: Symbol
                           , uoDirection   :: Direction
                           , uoOriginalQty :: Int
                           , uoQty         :: Int
                           , uoPrice       :: Int
                           , uoOrderType   :: OrderType
                           , uoOrderId     :: Int
                           , uoAccount     :: String
                           , uoTs          :: UTCTime
                           , uoFills       :: [Fill]
                           , uoTotalFilled :: Int
                           , uoOpen        :: Bool
                           } deriving Show

---- Quotes and fills
data Fill  = Fill  { fPrice     :: Int
                   , fQty       :: Int
                   , fTs        :: UTCTime
                   } deriving Show

data Quote = Quote { qSymbol    :: Symbol
                   , qVenue     :: String
                   , qBid       :: Maybe Int
                   , qAsk       :: Maybe Int
                   , qBidSize   :: Int
                   , qAskSize   :: Int
                   , qBidDepth  :: Int
                   , qAskDepth  :: Int
                   , qLastPrice :: Int
                   , qLastSize  :: Int
                   , qLastTrade :: UTCTime
                   , qQuoteTime :: UTCTime
                   } deriving Show

data Execution = Execution { eAccount          :: String
                           , eVenue            :: Venue
                           , eSymbol           :: Symbol
                           , eOrder            :: Order
                           , eStandingId       :: Int
                           , eIncomingId       :: Int
                           , ePrice            :: Int
                           , eFilled           :: Int
                           , eFilledAt         :: UTCTime
                           , eStandingComplete :: Bool
                           , eIncomingComplete :: Bool
                           } deriving Show

---- Misc

data    Stock  = Stock  { stockName :: String, stockSymbol :: Symbol } deriving Show
newtype Venue  = Venue  { unVenue  :: String } deriving Show
newtype Symbol = Symbol { unSymbol :: String } deriving Show

instance IsString Venue where
    fromString = Venue

instance IsString Symbol where
    fromString = Symbol


---- FromJSON

instance FromJSON Venue where
    parseJSON = withText "Venue" (pure . Venue . T.unpack)

instance FromJSON Symbol where
    parseJSON = withText "Symbol" (pure . Symbol . T.unpack)

instance FromJSON Direction where
    parseJSON = withText "Direction" $ \str ->
        case str of
          "buy"  -> return Bid
          "sell" -> return Ask
          _      -> empty

instance FromJSON OrderType where
    parseJSON = withText "OrderType" $ \str ->
        case str of
          "limit"               -> return Limit
          "market"              -> return Market
          "fill-or-kill"        -> return FillOrKill
          "immediate-or-cancel" -> return ImmediateOrCancel
          _                     -> empty

instance FromJSON Order where
    parseJSON = withObject "Order" $ \obj ->
        Order <$> (bool Ask Bid <$> obj .: "isBuy")
              <*>                   obj .: "price"
              <*>                   obj .: "qty"

instance FromJSON OrderBook where
    parseJSON = withObject "OrderBook" $ \obj ->
        OrderBook <$> obj .: "venue"
                  <*> obj .: "symbol"
                  <*> obj .: "bids"
                  <*> obj .: "asks"

instance FromJSON UserOrder where
    parseJSON = withObject "UserOrder" $ \obj ->
        UserOrder <$> obj .: "venue"
                  <*> obj .: "symbol"
                  <*> obj .: "direction"
                  <*> obj .: "originalQty"
                  <*> obj .: "qty"
                  <*> obj .: "price"
                  <*> obj .: "orderType"
                  <*> obj .: "id"
                  <*> obj .: "account"
                  <*> obj .: "ts"
                  <*> obj .: "fills"
                  <*> obj .: "totalFilled"
                  <*> obj .: "open"

instance FromJSON Fill where
    parseJSON = withObject "Fill" $ \obj ->
        Fill <$> obj .: "price"
             <*> obj .: "qty"
             <*> obj .: "ts"

instance FromJSON Quote where
    parseJSON = withObject "Quote" $ \obj ->
        Quote <$> obj .:  "symbol"
              <*> obj .:  "venue"
              <*> obj .:? "bid"
              <*> obj .:? "ask"
              <*> obj .:  "bidSize"
              <*> obj .:  "askSize"
              <*> obj .:  "bidDepth"
              <*> obj .:  "askDepth"
              <*> obj .:  "last"
              <*> obj .:  "lastSize"
              <*> obj .:  "lastTrade"
              <*> obj .:  "quoteTime"

instance FromJSON Execution where
    parseJSON = withObject "Execution" $ \obj ->
        Execution <$> obj .: "account"
                  <*> obj .: "venue"
                  <*> obj .: "symbol"
                  <*> obj .: "order"
                  <*> obj .: "standingId"
                  <*> obj .: "incomingId"
                  <*> obj .: "price"
                  <*> obj .: "filled"
                  <*> obj .: "filledAt"
                  <*> obj .: "standingComplete"
                  <*> obj .: "incomingComplete"

instance FromJSON Stock where
    parseJSON = withObject "Stock" $ \obj ->
        Stock <$> obj .: "name"
              <*> obj .: "symbol"
