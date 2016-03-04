{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Stockfighter.Types
  ( Direction(..)
  , OrderType(..)

  , Order(..)
  , OrderBook(..)
  , UserOrder(..)
  , Fill(..)
  , Quote(..)
  , Stock(..)

  -- Lenses
  , account
  , askDepth
  , askList
  , askSize
  , bidDepth
  , bidList
  , bidSize
  , direction
  , fills
  , lastPrice
  , lastSize
  , lastTrade
  , open
  , orderType
  , originalQty
  , price
  , qty
  , quoteTime
  , symbol
  , totalFilled
  , ts
  , venue
  ) where

import Control.Applicative ((<|>), empty)
import Control.Lens        (abbreviatedFields, makeLensesWith)
import Data.Aeson          ((.:), FromJSON(parseJSON), withObject, withText)
import Data.Aeson.Types    (Parser)
import Data.Bool           (bool)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format    (defaultTimeLocale, parseTimeM)


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

data OrderBook = OrderBook { obVenue       :: String
                           , obSymbol      :: String
                           , obBidList     :: [Order]
                           , obAskList     :: [Order]
                           } deriving Show

data UserOrder = UserOrder { uoVenue       :: String
                           , uoSymbol      :: String
                           , uoDirection   :: Direction
                           , uoOriginalQty :: Int
                           , uoQty         :: Int
                           , uoPrice       :: Int
                           , uoOrderType   :: OrderType
                           , uoOrderId     :: Int
                           , uoAccount     :: String
                           , uoTs          :: LocalTime
                           , uoFills       :: [Fill]
                           , uoTotalFilled :: Int
                           , uoOpen        :: Bool
                           } deriving Show

---- Quotes and fills
data Fill  = Fill  { fPrice     :: Int
                   , fQty       :: Int
                   , fTs        :: LocalTime
                   } deriving Show

data Quote = Quote { qSymbol    :: String
                   , qVenue     :: String
                   , qBid       :: Int
                   , qAsk       :: Int
                   , qBidSize   :: Int
                   , qAskSize   :: Int
                   , qBidDepth  :: Int
                   , qAskDepth  :: Int
                   , qLastPrice :: Int
                   , qLastSize  :: Int
                   , qLastTrade :: LocalTime
                   , qQuoteTime :: LocalTime
                   } deriving Show

---- Misc

data Stock = Stock { sName :: String, sSymbol :: String } deriving Show


parseTime :: String -> Parser LocalTime
parseTime s = parseTimeM True defaultTimeLocale "%FT%T%QZ"  s <|>
              parseTimeM True defaultTimeLocale "%FT%T%Q%z" s

---- Lenses

$(makeLensesWith abbreviatedFields ''Fill)
$(makeLensesWith abbreviatedFields ''Order)
$(makeLensesWith abbreviatedFields ''OrderBook)
$(makeLensesWith abbreviatedFields ''OrderType)
$(makeLensesWith abbreviatedFields ''Quote)
$(makeLensesWith abbreviatedFields ''Stock)
$(makeLensesWith abbreviatedFields ''UserOrder)

---- FromJSON

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
        UserOrder <$>                obj .: "venue"
                  <*>                obj .: "symbol"
                  <*>                obj .: "direction"
                  <*>                obj .: "originalQty"
                  <*>                obj .: "qty"
                  <*>                obj .: "price"
                  <*>                obj .: "orderType"
                  <*>                obj .: "id"
                  <*>                obj .: "account"
                  <*> (parseTime =<< obj .: "ts")
                  <*>                obj .: "fills"
                  <*>                obj .: "totalFilled"
                  <*>                obj .: "open"

instance FromJSON Fill where
    parseJSON = withObject "Fill" $ \obj ->
        Fill <$>                obj .: "price"
             <*>                obj .: "qty"
             <*> (parseTime =<< obj .: "ts")

instance FromJSON Quote where
    parseJSON = withObject "Quote" $ \obj ->
        Quote <$>                obj .: "symbol"
              <*>                obj .: "venue"
              <*>                obj .: "bid"
              <*>                obj .: "ask"
              <*>                obj .: "bidSize"
              <*>                obj .: "askSize"
              <*>                obj .: "bidDepth"
              <*>                obj .: "askDepth"
              <*>                obj .: "last"
              <*>                obj .: "lastSize"
              <*> (parseTime =<< obj .: "lastTrade")
              <*> (parseTime =<< obj .: "quoteTime")

instance FromJSON Stock where
    parseJSON = withObject "Stock" $ \obj ->
        Stock <$> obj .: "name"
              <*> obj .: "symbol"
