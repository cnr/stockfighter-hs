{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Stockfighter.Types
  ( Account(..)
  , ApiKey(..)
  , InstanceId(..)
  , SfLevel(..)
  , SfInst(..)

  , Venue(..)
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
import           Data.Aeson          ((.:), (.:?), FromJSON(parseJSON), Object, withObject, withScientific, withText)
import           Data.Bool           (bool)
import           Data.Hashable       (Hashable)
import           Data.Map            (Map)
import           Data.String         (IsString)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)


---- Stockfighter Level/Instance types
-- Too many types? Never!

newtype Account    = Account    { unAccount    :: String
                                } deriving (Eq, Ord, Show, Hashable, IsString)

newtype ApiKey     = ApiKey     { unApiKey     :: String
                                } deriving (Eq, Ord, Show, Hashable, IsString)

newtype InstanceId = InstanceId { unInstanceId :: Int
                                } deriving (Eq, Ord, Hashable, Show)

newtype Symbol     = Symbol     { unSymbol     :: String
                                } deriving (Eq, Ord, Show, Hashable, IsString)

newtype Venue      = Venue      { unVenue      :: String
                                } deriving (Eq, Ord, Show, Hashable, IsString)


data SfLevel = SfLevel { instanceId           :: InstanceId
                       , levelAccount         :: Account
                       , instructions         :: Map String String
                       , tickers              :: [Symbol]
                       , venues               :: [Venue]
                       , secondsPerTradingDay :: Int
                       , balances             :: Maybe (Map String Int)
                       } deriving Show

data SfInst = SfInst   { instDetails :: Object
                       , instDone    :: Bool
                       , instId      :: InstanceId
                       , instState   :: String
                       } deriving Show


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
                           , uoAccount     :: Account
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
                   , qVenue     :: Venue
                   , qBid       :: Maybe Int
                   , qAsk       :: Maybe Int
                   , qBidSize   :: Int
                   , qAskSize   :: Int
                   , qBidDepth  :: Int
                   , qAskDepth  :: Int
                   , qLastPrice :: Maybe Int
                   , qLastSize  :: Maybe Int
                   , qLastTrade :: Maybe UTCTime
                   , qQuoteTime :: UTCTime
                   } deriving Show

data Execution = Execution { eAccount          :: Account
                           , eVenue            :: Venue
                           , eSymbol           :: Symbol
                           , eOrder            :: UserOrder
                           , eStandingId       :: Int
                           , eIncomingId       :: Int
                           , ePrice            :: Int
                           , eFilled           :: Int
                           , eFilledAt         :: UTCTime
                           , eStandingComplete :: Bool
                           , eIncomingComplete :: Bool
                           } deriving Show

---- Misc

data Stock = Stock { stockName   :: String
                   , stockSymbol :: Symbol
                   } deriving Show


---- FromJSON

instance FromJSON Account where
    parseJSON = withText "Account" (pure . Account . T.unpack)

instance FromJSON ApiKey where
    parseJSON = withText "ApiKey" (pure . ApiKey . T.unpack)

instance FromJSON InstanceId where
    parseJSON = withScientific "ApiKey" (pure . InstanceId . floor)

instance FromJSON Symbol where
    parseJSON = withText "Symbol" (pure . Symbol . T.unpack)

instance FromJSON Venue where
    parseJSON = withText "Venue" (pure . Venue . T.unpack)

instance FromJSON SfLevel where
    parseJSON = withObject "SfLevel" $ \obj ->
        SfLevel <$> obj .:  "instanceId"
                <*> obj .:  "account"
                <*> obj .:  "instructions"
                <*> obj .:  "tickers"
                <*> obj .:  "venues"
                <*> obj .:  "secondsPerTradingDay"
                <*> obj .:? "balances"

instance FromJSON SfInst where
    parseJSON = withObject "SfInst" $ \obj ->
        SfInst <$> obj .: "details"
               <*> obj .: "done"
               <*> obj .: "id"
               <*> obj .: "state"

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
              <*> obj .:? "last"
              <*> obj .:? "lastSize"
              <*> obj .:? "lastTrade"
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
