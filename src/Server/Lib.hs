{-# LANGUAGE OverloadedStrings #-}

module Server.Lib ( startServer ) where

import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forM_, forever)
import Data.Text
import qualified Data.Text.IO as T
import Control.Exception (finally)

import qualified Server.Client as Client
import Text.Read (readMaybe)

type ServerState = Map.Map Client.Username Client.Client

newServerState :: ServerState
newServerState = Map.empty

receiveMessage :: WS.Connection -> IO Text
receiveMessage conn = WS.receiveData conn

readConnections :: MVar ServerState -> IO [WS.Connection]
readConnections state = do readMVar state >>= return . fmap Client.connection . Map.elems

addClient :: MVar ServerState -> Client.Client -> IO ()
addClient state client = modifyMVar_ state $ \s -> do 
    return $ Map.insert (Client.username client) client s
    
removeClient :: MVar ServerState -> Client.Client -> IO ()
removeClient state client = modifyMVar_ state $ \s -> do 
    return $ Map.delete (Client.username client) s

containsUsername :: MVar ServerState -> Client.Username -> IO Bool
containsUsername state user = do
    clients <- readMVar state
    return $ Map.member user clients

broadcast :: Text -> [WS.Connection] -> IO ()
broadcast message conns = do
    T.putStrLn message
    forM_ conns $ \c -> WS.sendTextData c message


handleClientMessage :: MVar ServerState -> Client.Client -> IO ()
handleClientMessage state client = do
    msg <- receiveMessage $ Client.connection client :: IO Text
    let message = (Client.usernameText client <> ": " <> msg)
    conns <- readConnections state
    broadcast message conns


startServer :: IO ()
startServer = do
    address <- Maybe.fromMaybe "0.0.0.0" <$> Environment.lookupEnv "SERVER_ADDRESS"
    port <-  Maybe.fromMaybe 1337 . (>>= readMaybe) <$> Environment.lookupEnv "SERVER_PORT"
    putStrLn $ "Starting server " ++ address ++ ":" ++ (show port)
    state <- newMVar newServerState
    WS.runServer address port $ \pending -> do
        conn <- WS.acceptRequest pending
        WS.withPingThread conn 30 (return ()) $ do
            username <- Client.Username <$> receiveMessage conn
            exists <- containsUsername state username
            if exists then do
                WS.sendClose conn ("User already exists" :: Text)
            else do
                conns <- readConnections state
                let client = Client.Client username conn
                broadcast (Client.usernameText client <> " joined") $ conns
                addClient state client
                flip finally (removeClient state client) $ forever $ handleClientMessage state client
