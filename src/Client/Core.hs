{-# LANGUAGE FlexibleInstances #-}
module Client.Core where

import qualified Network.WebSockets as WS
import Control.Monad.Reader (MonadIO(liftIO), asks, ReaderT)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text.IO as TIO

newtype ClientCtx = ClientCtx {
    getConn :: WS.Connection
}

type ClientEnv a = ReaderT ClientCtx IO a

class Monad m => Client m where
    recieveMessage :: Aeson.FromJSON a => m (Maybe a)
    sendMessage :: Aeson.ToJSON a => a -> m ()
    getUserLine :: m Text
    sendUserLine :: Text -> m ()

instance Client (ReaderT ClientCtx IO) where
    recieveMessage = do 
        conn <- asks getConn
        liftIO $ Aeson.decode <$> WS.receiveData conn
    sendMessage msg = do
        conn <- asks getConn
        liftIO $ WS.sendTextData conn (Aeson.encode msg)
    getUserLine = liftIO TIO.getLine
    sendUserLine = liftIO . TIO.putStrLn
