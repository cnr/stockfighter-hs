{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stockfighter.Client.Internal
  ( module Control.Monad.Except
  , module Control.Monad.Reader

  , StockfighterT
  , runStockfighter
  , APIError(..)
  , SfLevel(..)

  , get
  , getVenue
  , getVenueWith

  , post
  , postVenue

  , delete
  , deleteVenue

  , tape
  , tapeWith

  , SfResp(..) -- Not to be re-exported
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TMChan
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as L
import           Data.Map (Map)

import           Network.WebSockets
import qualified Network.Wreq       as W
import qualified Network.Wreq.Types as W
import           Stockfighter.Types
import           Wuss


gmBaseUrl :: String
gmBaseUrl = "https://api.stockfighter.io/gm/"

apiBaseUrl :: String
apiBaseUrl = "https://api.stockfighter.io/ob/api/"

wsBaseUrl :: String
wsBaseUrl = "api.stockfighter.io"

wsBasePath :: String
wsBasePath = "/ob/api/ws/"


---- StockfighterT

type StockfighterT m = ReaderT SfLevel (ExceptT APIError m)

runStockfighter :: SfLevel -> StockfighterT m a -> m (Either APIError a)
runStockfighter opts action = runExceptT (runReaderT action opts)


data APIError = HTTPError Int (Maybe String)
              | ParseError String
                deriving Show

data SfLevel = SfLevel { levelApiKey          :: String
                       , instanceId           :: Int
                       , levelAccount         :: String
                       , instructions         :: Map String String
                       , tickers              :: [Symbol]
                       , venues               :: [Venue]
                       , secondsPerTradingDay :: Int
                       , balances             :: Map String String
                       }


---- Responses

data SfResp = SfResp { respOk    :: Bool
                     , respError :: Maybe String
                     } deriving Show

instance FromJSON SfResp where
    parseJSON = withObject "SfResp" $ \obj ->
        SfResp <$> obj .:  "ok"
               <*> obj .:? "error"


---- GET requests

sfopts :: Monad m => StockfighterT m W.Options
sfopts = do
    apiKey <- asks levelApiKey
    return (W.defaults & W.header "X-Starfighter-Authorization" .~ [C8.pack apiKey])

-- Get and deserialize under the venue namespace
getVenue :: (MonadIO m, FromJSON a) => Venue -> String -> StockfighterT m a
getVenue venue path = get ("venues/" ++ unVenue venue ++ "/" ++ path)

-- Get under the venue namespace, deserializing a specific part of the returned JSON
getVenueWith :: (MonadIO m, FromJSON a) => Venue -> String -> (Value -> Maybe Value) -> StockfighterT m a
getVenueWith venue path sel = do
    resp <- getVenue venue path :: MonadIO m => StockfighterT m Value
    case fromJSON <$> sel resp of
        Nothing          -> throwError (ParseError "Missing expected field")
        Just (Error e)   -> throwError (ParseError e)
        Just (Success a) -> return a

-- Get and deserialize under the root namespace
get :: (MonadIO m, FromJSON a) => String -> StockfighterT m a
get s = do
    opts <- sfopts
    parseResponse =<< liftIO (W.getWith opts (apiBaseUrl ++ s))

getWith :: (MonadIO m, FromJSON a) => String -> (Value -> Maybe Value) -> StockfighterT m a
getWith path f = do
    resp <- get path
    case fromJSON <$> f resp of
        Nothing          -> throwError (ParseError "Missing expected field")
        Just (Error e)   -> throwError (ParseError e)
        Just (Success a) -> return a


---- POST requests

post :: (MonadIO m, W.Postable a, FromJSON r) => String -> a -> StockfighterT m r
post s a = do
    opts <- sfopts
    parseResponse =<< liftIO (W.postWith opts (apiBaseUrl ++ s) a)

postVenue :: (MonadIO m, W.Postable a, FromJSON r) => Venue -> String -> a -> StockfighterT m r
postVenue venue path = post ("venues/" ++ unVenue venue ++ "/" ++ path)


---- DELETE requests

delete :: (MonadIO m, FromJSON a) => String -> StockfighterT m a
delete s = do
    opts <- sfopts
    parseResponse =<< liftIO (W.deleteWith opts (apiBaseUrl ++ s))

deleteVenue :: (MonadIO m, FromJSON a) => Venue -> String -> StockfighterT m a
deleteVenue venue path = delete ("venues/" ++ unVenue venue ++ "/" ++ path)


---- WebSockets

tape :: (MonadIO m, FromJSON a) => String -> m (ThreadId, TMChan a)
tape s = tapeWith s Just

tapeWith :: (MonadIO m, FromJSON a) => String -> (Value -> Maybe Value) -> m (ThreadId, TMChan a)
tapeWith path f = liftIO $ runSecureClient wsBaseUrl 443 (wsBasePath ++ path) parseMessages
    where
    parseMessages :: FromJSON a => Connection -> IO (ThreadId, TMChan a)
    parseMessages conn = liftIO $ do
        chan <- atomically newTMChan

        let loop = do
              dataMessage <- receiveDataMessage conn
              let message = case dataMessage of
                                Binary m -> m -- just in case?
                                Text   m -> m
                  decoded = f =<< decode' message

              case fromJSON <$> decoded of
                  -- TODO: signal TMChan close reason?
                  Just (Success val) -> atomically (writeTMChan chan val) >> loop
                  _                  -> return ()

        tid <- forkIO (loop `finally` atomically (closeTMChan chan))
        return (tid, chan)


---- General

parseResponse :: (MonadIO m, FromJSON a) => W.Response L.ByteString -> StockfighterT m a
parseResponse response = do
    let maybeResp = eitherDecode' (response ^. W.responseBody) :: Either String SfResp
        code      = response ^. W.responseStatus . W.statusCode

    case maybeResp of
        Left e     -> throwError (ParseError e)
        Right resp -> unless (respOk resp) $
                          throwError (HTTPError code (respError resp))

    let maybeResult = eitherDecode' (response ^. W.responseBody)

    case maybeResult of
        Left e -> throwError (ParseError e)
        Right a -> return a
