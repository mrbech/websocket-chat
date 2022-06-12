{-# LANGUAGE OverloadedStrings #-}

module TuiClient.Main where

import qualified Brick.BChan as C
import Control.Concurrent (forkIO)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), void, when)
import qualified Data.Maybe as Maybe
import qualified Network.WebSockets as WS
import qualified System.Environment as Environment
import Text.Read (readMaybe)
import TuiClient.Core (Client (..), ClientEnv, UiClientCtx (..), UiEvent (SystemEvent))
import TuiClient.Lib
import qualified TuiClient.Ui as Ui

fork :: ClientEnv () -> ClientEnv ()
fork f = do
  ctx <- ask
  liftIO $ void $ forkIO $ runReaderT f ctx

whileM :: Monad m => m Bool -> m ()
whileM f = do
  b <- f
  when b (whileM f)

backgroundHandler :: ClientEnv ()
backgroundHandler = do
  {- Handle the initiation process, requesting username etc. -}
  connectedHandler

  {- Listen for messages in the background -}
  fork (whileM listenForMessage)

  {- Read and send message loop -}
  readSendLoop

mainHandler :: ClientEnv ()
mainHandler = do
  sendUiEvent $ SystemEvent "Connected!"

  fork backgroundHandler

  Ui.run

  return ()

startClient :: IO ()
startClient = do
  eventChan <- C.newBChan 10
  inputChan <- C.newBChan 10
  address <- Maybe.fromMaybe "0.0.0.0" <$> Environment.lookupEnv "SERVER_ADDRESS"
  port <- Maybe.fromMaybe 1337 . (>>= readMaybe) <$> Environment.lookupEnv "SERVER_PORT"
  putStrLn $ "Connecting to " ++ address ++ ":" ++ show port
  WS.runClient address port "/" $ \conn -> runReaderT mainHandler (UiClientCtx {getConn = conn, getEventChan = eventChan, getInputChan = inputChan})
