{-# LANGUAGE FlexibleInstances #-}
module Client.Core where

import qualified Network.WebSockets as WS
import Control.Monad.Reader (MonadIO(liftIO), asks, ReaderT)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Brick.BChan as C

type ClientEnv a = ReaderT UiClientCtx IO a

class Monad m => Client m where
    recieveMessage :: Aeson.FromJSON a => m (Maybe a)
    sendMessage :: Aeson.ToJSON a => a -> m ()
    getUserLine :: m Text
    sendUserLine :: Text -> m ()

data UiClientCtx = UiClientCtx {
    getConn :: WS.Connection,
    getInputChan :: C.BChan Text,
    getEventChan :: C.BChan Text
}

instance Client (ReaderT UiClientCtx IO) where
    recieveMessage = do 
        conn <- asks getConn
        liftIO $ Aeson.decode <$> WS.receiveData conn
    sendMessage msg = do
        conn <- asks getConn
        liftIO $ WS.sendTextData conn (Aeson.encode msg)
    getUserLine = do
        inputChan <- asks getInputChan
        liftIO $ C.readBChan inputChan
    sendUserLine text = do
        eventChan <- asks getEventChan
        liftIO $ C.writeBChan eventChan text
