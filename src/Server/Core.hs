{-# LANGUAGE FlexibleInstances #-}
module Server.Core where

import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS
import Control.Monad.Reader (forM_, MonadIO(liftIO), asks, ReaderT)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Control.Concurrent (newMVar, modifyMVar_, modifyMVar, readMVar, MVar)
import qualified Data.Text.IO as T
import Data.IORef (readIORef, writeIORef, newIORef, IORef)

type ServerState = MVar (Map.Map Text WS.Connection)

initState :: IO ServerState
initState = newMVar Map.empty

data ClientConnectionCtx = ClientConnectionCtx {
    getUsername :: IORef (Maybe Text),
    getConn :: WS.Connection,
    getServerState :: ServerState
}

createCtx :: ServerState -> WS.Connection -> IO ClientConnectionCtx
createCtx state conn = do
    username <- newIORef Nothing
    return ClientConnectionCtx { getUsername = username, getConn = conn, getServerState = state }

type ClientConnectionEnv a = ReaderT ClientConnectionCtx IO a

class Monad m => ClientConnection m where
    recieveMessage :: Aeson.FromJSON a => m (Maybe a)
    sendMessage :: Aeson.ToJSON a => a -> m ()
    broadcast :: Aeson.ToJSON a => a -> m ()
    tryJoin :: Text -> m Bool
    logInfo :: Text -> m ()
    leave :: m ()

instance ClientConnection (ReaderT ClientConnectionCtx IO) where
    recieveMessage = do 
        conn <- asks getConn
        liftIO $ Aeson.decode <$> WS.receiveData conn

    sendMessage msg = do
        conn <- asks getConn
        liftIO $ WS.sendTextData conn (Aeson.encode msg)

    broadcast msg = do
        state <- asks getServerState
        conns <- liftIO $ Map.elems <$> readMVar state
        forM_ conns $ \c -> liftIO $ WS.sendTextData c (Aeson.encode msg)

    tryJoin username = do
        state <- asks getServerState
        conn <- asks getConn
        result <- liftIO $ modifyMVar state $ \s ->
            if Map.member username s then
                return (s, False)
            else
                return (Map.insert username conn s, True)
        if result then do
            u <- asks getUsername
            liftIO $ writeIORef u (Just username)
            return result
        else
            return result

    logInfo = liftIO . T.putStrLn

    leave = do
        state <- asks getServerState
        username <- asks getUsername
        u <- liftIO $ readIORef username
        case u of
            Nothing -> return ()
            Just u' -> liftIO $ modifyMVar_ state $ return . Map.delete u'
