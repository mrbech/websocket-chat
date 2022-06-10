{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client.Core where

import qualified Brick.BChan as C
import Control.Exception (SomeException, catch, displayException)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT, asks)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Messages.Server as SM
import qualified Network.WebSockets as WS

-- |
-- Events that changes the current Ui
data UiEvent
  = SystemEvent Text
  | ErrorEvent Text
  | ChatEvent SM.Chat
  deriving (Show)

class Monad m => Client m where
  recieveMessage :: Aeson.FromJSON a => m (Either String a)
  sendMessage :: Aeson.ToJSON a => a -> m ()
  getUserLine :: m Text
  sendUiEvent :: UiEvent -> m ()

type ClientEnv a = ReaderT UiClientCtx IO a

data UiClientCtx = UiClientCtx
  { getConn :: WS.Connection,
    getInputChan :: C.BChan Text,
    getEventChan :: C.BChan UiEvent
  }

instance Client (ReaderT UiClientCtx IO) where
  recieveMessage = do
    conn <- asks getConn
    liftIO $
      (maybe (Left "Failed to decode message") Right . Aeson.decode <$> WS.receiveData conn)
        `catch` (\(e :: SomeException) -> return $ Left (displayException e))

  sendMessage msg = do
    conn <- asks getConn
    liftIO $ WS.sendTextData conn (Aeson.encode msg)
  getUserLine = do
    inputChan <- asks getInputChan
    liftIO $ C.readBChan inputChan
  sendUiEvent event = do
    eventChan <- asks getEventChan
    liftIO $ C.writeBChan eventChan event
