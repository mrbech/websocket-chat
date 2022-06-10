{-# LANGUAGE OverloadedStrings #-}

module Client.Main where

import qualified Brick.BChan as C
import Client.Core (Client (..), ClientEnv, UiClientCtx (..), UiEvent (SystemEvent))
import Client.Lib
import qualified Client.Ui as Ui
import Control.Concurrent (forkIO)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), forever, void)
import qualified Data.Maybe as Maybe
import qualified Network.WebSockets as WS
import qualified System.Environment as Environment
import Text.Read (readMaybe)

fork :: ClientEnv () -> ClientEnv ()
fork f = do
  ctx <- ask
  liftIO $ void $ forkIO $ runReaderT f ctx

backgroundHandler :: ClientEnv ()
backgroundHandler = do
  {- Handle the initiation process, requesting username etc. -}
  connectedHandler

  {- Listen for messages in the background -}
  fork (forever listenForMessage)

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
