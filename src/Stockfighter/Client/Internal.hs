{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}

module Stockfighter.Client.Internal
  ( MonadAPI
  , APIException(..)
  , InstanceId(..)
  , Account(..)

  , getGM
  , getGMWith
  , getAPI
  , getAPIWith

  , postGM
  , postAPI

  , deleteAPI

  , tape
  , tapeWith

  , SfResp(..) -- Not to be re-exported
  ) where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as L
import           Data.List
import           Network.WebSockets
import qualified Network.Wreq       as W
import qualified Network.Wreq.Types as W
import           Stockfighter.Types
import           Wuss


gmBaseUrl :: String
gmBaseUrl = "https://api.stockfighter.io/gm"

apiBaseUrl :: String
apiBaseUrl = "https://api.stockfighter.io/ob/api"

wsBaseUrl :: String
wsBaseUrl = "api.stockfighter.io"

wsBasePath :: String
wsBasePath = "/ob/api/ws"


---- MonadAPI

type MonadAPI m = (MonadIO m, MonadThrow m, MonadReader ApiKey m)

data APIException = NotOK Int (Maybe String)
                  | ParseError String
                    deriving Show

instance Exception APIException


---- Responses

data SfResp = SfResp { respOk    :: Bool
                     , respError :: Maybe String
                     } deriving Show

instance FromJSON SfResp where
    parseJSON = withObject "SfResp" $ \obj ->
        SfResp <$> obj .:  "ok"
               <*> obj .:? "error"


---- Wreq options

mkOpts :: String -> W.Options
mkOpts apiKey = W.defaults & W.header "X-Starfighter-Authorization" .~ [C8.pack apiKey]


---- GET requests

getBase :: (MonadAPI m, FromJSON a) => [String] -> (Value -> Maybe Value) -> m a
getBase path sel = do
    (ApiKey apiKey) <- ask
    resp <- parseResponse =<< liftIO (W.getWith (mkOpts apiKey) (intercalate "/" path))
    case fromJSON <$> sel resp of
        Nothing          -> throwM (ParseError "Missing expected field")
        Just (Error e)   -> throwM (ParseError e)
        Just (Success a) -> return a

getGM :: (MonadAPI m, FromJSON a) => [String] -> m a
getGM path = getGMWith path Just

getGMWith :: (MonadAPI m, FromJSON a) => [String] -> (Value -> Maybe Value) -> m a
getGMWith path = getBase (gmBaseUrl : path)

getAPI :: (MonadAPI m, FromJSON a) => [String] -> m a
getAPI path = getAPIWith path Just

getAPIWith :: (MonadAPI m, FromJSON a) => [String] -> (Value -> Maybe Value) -> m a
getAPIWith path = getBase (apiBaseUrl : path)


---- POST requests

postBase :: (MonadAPI m, W.Postable p, FromJSON r) => [String] -> p -> m r
postBase path p = do
    (ApiKey apiKey) <- ask
    parseResponse =<< liftIO (W.postWith (mkOpts apiKey) (intercalate "/" path) p)

postGM :: (MonadAPI m, W.Postable p, FromJSON r) => [String] -> p -> m r
postGM path = postBase (gmBaseUrl : path)

postAPI :: (MonadAPI m, W.Postable p, FromJSON r) => [String] -> p -> m r
postAPI path = postBase (apiBaseUrl : path)


---- DELETE requests

deleteAPI :: (MonadAPI m, FromJSON a) => [String] -> m a
deleteAPI path = do
    (ApiKey apiKey) <- ask
    parseResponse =<< liftIO (W.deleteWith (mkOpts apiKey) (intercalate "/" (apiBaseUrl : path)))


---- WebSockets

tape :: (MonadIO m, FromJSON a) => [String] -> (a -> IO ()) -> m ThreadId
tape path = tapeWith path Just

tapeWith :: (MonadIO m, FromJSON a) => [String] -> (Value -> Maybe Value) -> (a -> IO ()) -> m ThreadId
tapeWith path sel f = liftIO $ runSecureClient wsBaseUrl 443 (intercalate "/" (wsBasePath : path)) parseMessages
    where
    parseMessages :: MonadIO m => Connection -> m ThreadId
    parseMessages conn = do
        let
            loop = do
              dataMessage <- liftIO $ receiveDataMessage conn
              let message = case dataMessage of
                                Binary m -> m -- just in case?
                                Text   m -> m
                  decoded = sel =<< decode' message

              case fromJSON <$> decoded of
                  -- TODO: signal TMChan close reason?
                  Just (Success val) -> f val >> loop
                  _                  -> return ()

        liftIO (forkIO loop)


---- General

parseResponse :: (MonadAPI m, FromJSON a) => W.Response L.ByteString -> m a
parseResponse response = do
    let maybeResp = eitherDecode' (response ^. W.responseBody) :: Either String SfResp
        code      = response ^. W.responseStatus . W.statusCode

    case maybeResp of
        Left e     -> throwM (ParseError e)
        Right resp -> unless (respOk resp) $
                          throwM (NotOK code (respError resp))

    let maybeResult = eitherDecode' (response ^. W.responseBody)

    case maybeResult of
        Left e -> throwM (ParseError e)
        Right a -> return a
