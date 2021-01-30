{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.Lib (startClient) where

import qualified Network.WebSockets as WS
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import Text.Read (readMaybe)
import Control.Concurrent (forkIO, killThread)
import qualified Data.Text.IO as T
import Control.Monad (forever)
import qualified Messages.Server as SM
import qualified Messages.Client as CM

listenForMessage :: WS.Connection -> IO ()
listenForMessage conn = do 
    msg <- WS.receiveData conn
    case SM.decodeChat msg of
        Nothing -> T.putStrLn $ "Failed to decode Chat: " <> msg
        Just (SM.ChatMessage SM.Message { SM.fromUsername, SM.message }) -> 
            T.putStrLn (fromUsername <> ": " <> message)
        Just (SM.ChatUserJoined SM.UserJoined { SM.usernameJoined }) -> 
            T.putStrLn (usernameJoined <> " joined")
        Just (SM.ChatUserLeft SM.UserLeft { SM.usernameLeft }) -> 
            T.putStrLn (usernameLeft <> " left")

readSendLoop :: WS.Connection -> IO ()
readSendLoop conn = do
    msg <- T.getLine
    if msg == ":quit" then
        return ()
    else do
        WS.sendTextData conn $ CM.encodeChatMessage $ CM.ChatMessage msg

        readSendLoop conn

requestUsernameHandler :: WS.Connection -> IO ()
requestUsernameHandler conn = do
    T.putStrLn "Please enter your name:"
    username <- T.getLine
    WS.sendTextData conn $ CM.encodeRequestUsername $ CM.RequestUsername username
    res <- WS.receiveData conn
    case SM.decodeRequestUsernameResponse res of
        Nothing ->
            T.putStrLn $ "Failed to decode RequestUsernameResponse: " <> res
        Just SM.UsernameAccepted -> 
            T.putStrLn "Username accepted, welcome to the chat!"
        Just SM.UsernameAlreadyTaken -> do
            T.putStrLn "Name already taken, please try again"
            requestUsernameHandler conn

connectedHandler :: WS.Connection -> IO ()
connectedHandler conn = do
    msg <- WS.receiveData conn
    case SM.decodeUserConnected msg of
        Nothing -> 
            T.putStrLn $ "Failed to decode user connected: " <> msg
        Just SM.RequestUsername ->
            requestUsernameHandler conn

startClient :: IO ()
startClient = do
    address <- Maybe.fromMaybe "0.0.0.0" <$> Environment.lookupEnv "SERVER_ADDRESS"
    port <- Maybe.fromMaybe 1337 . (>>= readMaybe) <$> Environment.lookupEnv "SERVER_PORT"
    putStrLn $ "Connecting to " ++ address ++ ":" ++ show port
    WS.runClient address port "/" $ \conn -> do
        putStrLn "Connected!"

        {- Handle the initiation process, requesting username etc. -}
        connectedHandler conn

        {- Listen for messages in the background -}
        backgroundThread <- forkIO $ forever $ listenForMessage conn

        {- Read and send message loop -}
        readSendLoop conn

        killThread backgroundThread
        
        return ()
