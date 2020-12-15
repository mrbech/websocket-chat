{-# LANGUAGE OverloadedStrings #-}
module Client.Lib (startClient) where

import qualified Network.WebSockets as WS
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import Text.Read (readMaybe)
import Control.Concurrent (forkIO, killThread)
import qualified Data.Text.IO as T
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(liftIO))

listenForMessage :: WS.Connection -> IO ()
listenForMessage conn = do 
    msg <- WS.receiveData conn
    liftIO $ T.putStrLn msg

readSendLoop :: WS.Connection -> IO ()
readSendLoop conn = do
    msg <- T.getLine
    if msg == ":quit" then
        return ()
    else do
        WS.sendTextData conn msg
        readSendLoop conn

startClient :: IO ()
startClient = do
    address <- Maybe.fromMaybe "0.0.0.0" <$> Environment.lookupEnv "SERVER_ADDRESS"
    port <-  Maybe.fromMaybe 1337 . (>>= readMaybe) <$> Environment.lookupEnv "SERVER_PORT"
    putStrLn $ "Connecting to " ++ address ++ ":" ++ show port
    WS.runClient address port "/" $ \conn -> do
        putStrLn "Connected!"
        putStrLn "Welcome, please enter your name:"

        {- Listen for messages in the background -}
        backgroundThread <- forkIO $ forever $ listenForMessage conn

        {- First we send username -}
        name <- T.getLine
        WS.sendTextData conn name

        {- Read and send message loop -}
        readSendLoop conn

        killThread backgroundThread
        
        return ()
