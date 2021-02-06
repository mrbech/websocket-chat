{-# LANGUAGE OverloadedStrings #-}
module Client.Main where

import qualified Network.WebSockets as WS
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import Text.Read (readMaybe)
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forever)
import Control.Monad.Reader (MonadReader(ask), liftIO, ReaderT(runReaderT))
import Client.Core (ClientCtx(..), ClientEnv, Client(..))
import Client.Lib

mainHandler :: ClientEnv ()
mainHandler = do
    sendUserLine "Connected!"

    {- Handle the initiation process, requesting username etc. -}
    connectedHandler

    {- Listen for messages in the background -}
    ctx <- ask
    backgroundThread <- liftIO $ forkIO $ runReaderT (forever listenForMessage) ctx

    {- Read and send message loop -}
    readSendLoop

    liftIO $ killThread backgroundThread
    
    return ()

startClient :: IO ()
startClient = do
    address <- Maybe.fromMaybe "0.0.0.0" <$> Environment.lookupEnv "SERVER_ADDRESS"
    port <- Maybe.fromMaybe 1337 . (>>= readMaybe) <$> Environment.lookupEnv "SERVER_PORT"
    putStrLn $ "Connecting to " ++ address ++ ":" ++ show port
    WS.runClient address port "/" $ \conn -> runReaderT mainHandler (ClientCtx { getConn = conn }) 
