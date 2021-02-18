{-# LANGUAGE FlexibleInstances #-}
module Server.Core where

import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS
import Control.Monad.Reader (forM_, MonadIO(liftIO), asks, ReaderT)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Control.Concurrent (newMVar, modifyMVar_, modifyMVar, readMVar, MVar)
import Data.IORef (readIORef, writeIORef, newIORef, IORef)
import Server.Log (Logging(..))
import System.IO (hPrint, stderr)

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
    leave :: m ()
    connectedUsers :: m [Text]

instance ClientConnection (ReaderT ClientConnectionCtx IO) where
    recieveMessage = do 
        conn <- asks getConn
        msg <- liftIO $ WS.receiveData conn
        logInfo ("recieveMessage", msg)
        return $ Aeson.decode msg

    sendMessage msg = do
        conn <- asks getConn
        let m = Aeson.encode msg
        logInfo ("sendMessage", m)
        liftIO $ WS.sendTextData conn m

    broadcast msg = do
        state <- asks getServerState
        conns <- liftIO $ Map.elems <$> readMVar state
        let m = Aeson.encode msg
        logInfo ("broadcast", m)
        forM_ conns $ \c -> liftIO $ WS.sendTextData c m

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

    leave = do
        state <- asks getServerState
        username <- asks getUsername
        u <- liftIO $ readIORef username
        case u of
            Nothing -> return ()
            Just u' -> liftIO $ modifyMVar_ state $ return . Map.delete u'

    connectedUsers = do
        state <- asks getServerState
        s <- liftIO $ readMVar state
        return $ Map.keys s

instance Logging (ReaderT ClientConnectionCtx IO) where
    printStdout msg = do
        username <- asks getUsername
        u <- liftIO $ readIORef username
        liftIO . print $ (u, msg)
    printStderr msg = do
        username <- asks getUsername
        u <- liftIO $ readIORef username
        liftIO . hPrint stderr $ (u, msg)
