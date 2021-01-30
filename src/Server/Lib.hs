{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Lib ( startServer ) where

import qualified Network.WebSockets as WS
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forM_, forever)
import Data.Text
import qualified Data.Text.IO as T
import Control.Exception (finally)

import qualified Server.Client as Client
import qualified Server.ServerState as ServerState
import Text.Read (readMaybe)
import Data.Functor ((<&>))
import qualified Messages.Server as SM
import qualified Messages.Client as CM

type State = MVar ServerState.ServerState

receiveMessage :: WS.Connection -> IO Text
receiveMessage = WS.receiveData

readConnections :: State -> IO [WS.Connection]
readConnections state = readMVar state <&> ServerState.connections

addClient :: State -> Client.Client -> IO ()
addClient state client = modifyMVar_ state $ return . ServerState.addClient client
    
removeClient :: State -> Client.Client -> IO ()
removeClient state client = modifyMVar_ state $ return . ServerState.removeClient client

containsUsername :: State -> Client.Username -> IO Bool
containsUsername state user = readMVar state <&> ServerState.containsUsername user

broadcast :: Text -> State -> IO ()
broadcast message state = do
    T.putStrLn message
    conns <- readConnections state
    forM_ conns $ \c -> WS.sendTextData c message

handleClientMessage :: State -> Client.Client -> IO ()
handleClientMessage state client = do
    msg <- receiveMessage $ Client.connection client
    case CM.decodeChatMessage msg of
        Nothing -> T.putStrLn $ "Failed to decode ChatMessage: " <> msg
        Just CM.ChatMessage { CM.message } -> broadcastClientMessage message client state

broadcastClientMessage :: Text -> Client.Client -> State -> IO ()
broadcastClientMessage message client = broadcast
    $ SM.encodeChat
    $ SM.ChatMessage
    $ SM.Message { SM.fromUsername, SM.message }
    where fromUsername = Client.usernameText client

broadcastClientJoin :: Client.Client -> State -> IO ()
broadcastClientJoin client = broadcast 
    $ SM.encodeChat 
    $ SM.ChatUserJoined 
    $ SM.UserJoined { SM.usernameJoined } 
    where usernameJoined = Client.usernameText client

broadcastClientLeave :: Client.Client -> State -> IO ()
broadcastClientLeave client = broadcast
    $ SM.encodeChat 
    $ SM.ChatUserLeft
    $ SM.UserLeft { SM.usernameLeft } 
    where usernameLeft = Client.usernameText client

getUsername :: WS.Connection -> State -> IO (Maybe Client.Username)
getUsername conn state = do
    WS.sendTextData conn $ SM.encodeUserConnected SM.RequestUsername
    getUsername'
    where 
        getUsername' = do
            msg <- receiveMessage conn
            case CM.decodeRequestUsername msg of
                Nothing -> do
                    T.putStrLn $ "Failed to decode RequestUsername: " <> msg
                    return Nothing
                Just CM.RequestUsername { CM.username } -> do
                    let u = Client.Username username
                    exists <- containsUsername state u
                    if exists then do
                        WS.sendTextData conn $ SM.encodeRequestUsernameResponse SM.UsernameAlreadyTaken
                        getUsername'
                    else do
                        WS.sendTextData conn $ SM.encodeRequestUsernameResponse SM.UsernameAccepted
                        return $ Just u 

startServer :: IO ()
startServer = do
    address <- Maybe.fromMaybe "0.0.0.0" <$> Environment.lookupEnv "SERVER_ADDRESS"
    port <- Maybe.fromMaybe 1337 . (>>= readMaybe) <$> Environment.lookupEnv "SERVER_PORT"
    putStrLn $ "Starting server " ++ address ++ ":" ++ show port
    state <- newMVar ServerState.newServerState
    WS.runServer address port $ \pending -> do
        conn <- WS.acceptRequest pending
        WS.withPingThread conn 30 (return ()) $ do
            username <- getUsername conn state
            case username of
                Nothing -> return ()
                Just u -> do
                    let client = Client.Client u conn
                    broadcastClientJoin client state
                    addClient state client
                    finally (forever $ handleClientMessage state client)
                        (removeClient state client >> broadcastClientLeave client state)
