{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stockfighter.Client.Internal
  ( module Control.Monad.Except
  , module Control.Monad.Reader

  , StockfighterT
  , runStockfighter
  , APIError(..)
  , SfOpts(..)

  , get
  , getVenue
  , getVenueWith

  , post
  , postVenue

  , delete
  , deleteVenue

  , Tape
  , EndTape(..)
  , tape
  , tapeWith

  , SfResp(..) -- Not to be re-exported
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as L
import           Network.WebSockets
import qualified Network.Wreq       as W
import qualified Network.Wreq.Types as W
import           Wuss


apiBaseUrl :: String
apiBaseUrl = "https://api.stockfighter.io/ob/api/"

wsBaseUrl :: String
wsBaseUrl = "api.stockfighter.io"

wsBasePath :: String
wsBasePath = "/ob/api/ws/"


---- StockfighterT

type StockfighterT m = ReaderT SfOpts (ExceptT APIError m)

runStockfighter :: SfOpts -> StockfighterT m a -> m (Either APIError a)
runStockfighter opts action = runExceptT (runReaderT action opts)


data APIError = HTTPError Int (Maybe String)
              | ParseError String
                deriving Show

data SfOpts = SfOpts { optApiKey  :: String
                     , optAccount :: String
                     , optVenue   :: String
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
    apiKey <- asks optApiKey
    return (W.defaults & W.header "X-Starfighter-Authorization" .~ [C8.pack apiKey])

-- Get and deserialize under the venue namespace
getVenue :: (MonadIO m, FromJSON a) => String -> StockfighterT m a
getVenue s = do
    _venue <- asks optVenue
    get ("venues/" ++ _venue ++ "/" ++ s)

-- Get under the venue namespace, deserializing a specific part of the returned JSON
getVenueWith :: (MonadIO m, FromJSON a) => String -> (Value -> Maybe Value) -> StockfighterT m a
getVenueWith s f = do
    resp <- getVenue s :: MonadIO m => StockfighterT m Value
    case fromJSON <$> f resp of
        Nothing          -> throwError (ParseError "Missing expected field")
        Just (Error e)   -> throwError (ParseError e)
        Just (Success a) -> return a

-- Get and deserialize under the root namespace
get :: (MonadIO m, FromJSON a) => String -> StockfighterT m a
get s = do
    opts <- sfopts
    parseResponse =<< liftIO (W.getWith opts (apiBaseUrl ++ s))


---- POST requests

post :: (MonadIO m, W.Postable a, FromJSON r) => String -> a -> StockfighterT m r
post s a = do
    opts <- sfopts
    parseResponse =<< liftIO (W.postWith opts (apiBaseUrl ++ s) a)


postVenue :: (MonadIO m, W.Postable a, FromJSON r) => String -> a -> StockfighterT m r
postVenue s a = do
    _venue <- asks optVenue
    post ("venues/" ++ _venue ++ "/" ++ s) a


---- DELETE requests

delete :: (MonadIO m, FromJSON a) => String -> StockfighterT m a
delete s = do
    opts <- sfopts
    parseResponse =<< liftIO (W.deleteWith opts (apiBaseUrl ++ s))

deleteVenue :: (MonadIO m, FromJSON a) => String -> StockfighterT m a
deleteVenue s = do
    _venue <- asks optVenue
    delete ("venues/" ++ _venue ++ "/" ++ s)


---- WebSockets

type Tape a = Chan (Either EndTape a)

data EndTape = MessageNotJSON
             | MissingValue
             | FailedDecode String
             | SocketClosed
             | Killed
             | IOE IOException
               deriving Show


tape :: (MonadIO m, FromJSON a) => String -> m (ThreadId, Tape a)
tape s = tapeWith s Just

tapeWith :: (MonadIO m, FromJSON a) => String -> (Value -> Maybe Value) -> m (ThreadId, Tape a)
tapeWith path f = liftIO $ runSecureClient wsBaseUrl 443 (wsBasePath ++ path) parseMessages -- TODO: handle connect exceptions, close connection properly

    where
    parseMessages :: FromJSON a => Connection -> IO (ThreadId, Tape a)
    parseMessages conn = liftIO $ do
        chan <- newChan

        let loop = (do
              dataMessage <- receiveDataMessage conn
              let message = case dataMessage of
                                Binary m -> m
                                Text   m -> m
                  decoded = eitherDecode' message :: Either String Value

              case decoded of
                  Left  _ -> writeChan chan (Left MessageNotJSON)
                  Right a -> case fromJSON <$> f a of
                      Nothing            -> writeChan chan (Left MissingValue)
                      Just (Error e)     -> writeChan chan (Left (FailedDecode e))
                      Just (Success val) -> writeChan chan (Right val) >> loop)
                `catch` \ThreadKilled        -> writeChan chan (Left Killed)
                `catch` \ConnectionClosed    -> writeChan chan (Left SocketClosed)
                `catch` \(ParseException s)  -> writeChan chan (Left (FailedDecode s))
                `catch` \(ex :: IOException) -> writeChan chan (Left (IOE ex))

        tid <- forkIO loop
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
