{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Lib where

import Data.Text (Text)
import qualified Messages.Client as CM
import qualified Messages.Server as SM
import Server.Core (ClientConnection (..))
import Server.Log (Logging (..))

clientJoin :: (ClientConnection m, Logging m) => m (Maybe Text)
clientJoin = do
  sendMessage SM.RequestUsername
  join'
  where
    join' = do
      msg <- recieveMessage
      case msg of
        Nothing -> do
          logErrorT "Failed to decode RequestUsername"
          return Nothing
        Just CM.RequestUsername {CM.username} -> do
          success <- tryJoin username
          if success
            then do
              users <- connectedUsers
              sendMessage SM.UsernameAccepted
              broadcast $ SM.ChatUserJoined $ SM.UserJoined {SM.usernameJoined = username}
              sendMessage $ SM.Sync users
              return $ Just username
            else do
              sendMessage SM.UsernameAlreadyTaken
              join'

handleClientMessage :: (ClientConnection m, Logging m) => Text -> m ()
handleClientMessage username = do
  msg <- recieveMessage
  case msg of
    Nothing -> logErrorT "Failed to decode ChatMessage"
    Just CM.ChatMessage {CM.message} ->
      broadcast $
        SM.ChatMessage $
          SM.Message {SM.fromUsername = username, SM.message}

handleClientLeave :: ClientConnection m => Text -> m ()
handleClientLeave username = do
  leave
  broadcast $ SM.ChatUserLeft $ SM.UserLeft {SM.usernameLeft = username}
