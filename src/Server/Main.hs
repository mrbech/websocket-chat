{-# LANGUAGE OverloadedStrings #-}

module Server.Main ( startServer ) where

import qualified Network.WebSockets as WS
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import Control.Monad (forever)
import Control.Exception (finally)

import Text.Read (readMaybe)
import qualified Messages.Server as SM
import Server.Lib
import Server.Core
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Server.Log (Logging(logInfo))

handleClientConnection :: ClientConnectionEnv ()
handleClientConnection = do
    username <- clientJoin
    case username of
        Nothing -> return ()
        Just u -> do
            ctx <- ask
            broadcast $ SM.ChatUserJoined $ SM.UserJoined { SM.usernameJoined = u }
            liftIO $ finally 
                (forever $ runReaderT (handleClientMessage u) ctx)
                (runReaderT (handleClientLeave u) ctx)

startServer :: IO ()
startServer = do
    address <- Maybe.fromMaybe "0.0.0.0" <$> Environment.lookupEnv "SERVER_ADDRESS"
    port <- Maybe.fromMaybe 1337 . (>>= readMaybe) <$> Environment.lookupEnv "SERVER_PORT"
    logInfo $ "Starting server " ++ address ++ ":" ++ show port
    state <- initState
    WS.runServer address port $ \pending -> do
        conn <- WS.acceptRequest pending
        ctx <- createCtx state conn
        WS.withPingThread conn 30 (return ()) $ runReaderT handleClientConnection ctx 
