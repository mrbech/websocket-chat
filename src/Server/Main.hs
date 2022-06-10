{-# LANGUAGE OverloadedStrings #-}

module Server.Main (startServer) where

import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import qualified Data.Maybe as Maybe
import qualified Network.WebSockets as WS
import Server.Core
import Server.Lib
import Server.Log (Logging (logInfo))
import qualified System.Environment as Environment
import Text.Read (readMaybe)

handleClientConnection :: ClientConnectionEnv ()
handleClientConnection = do
  username <- clientJoin
  case username of
    Nothing -> return ()
    Just u -> do
      ctx <- ask
      liftIO $
        finally
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
