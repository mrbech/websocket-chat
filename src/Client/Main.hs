{-# LANGUAGE OverloadedStrings #-}
module Client.Main where

import qualified Network.WebSockets as WS
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import Text.Read (readMaybe)
import Control.Monad.Reader (forever, void, MonadReader(ask), MonadIO(liftIO), ReaderT(runReaderT))
import Client.Core (UiClientCtx(..), ClientEnv, Client(..))
import Client.Lib
import qualified Client.Ui as Ui
import qualified Brick.BChan as C
import Control.Concurrent (forkIO)

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
    sendUserLine "Connected!"

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
    WS.runClient address port "/" $ \conn -> runReaderT mainHandler (UiClientCtx { getConn = conn, getEventChan = eventChan, getInputChan = inputChan }) 
