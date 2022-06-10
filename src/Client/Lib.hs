{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Lib where

import Client.Core (Client (..), UiEvent (..))
import qualified Messages.Client as CM
import qualified Messages.Server as SM

listenForMessage :: Client m => m ()
listenForMessage = do
  msg <- recieveMessage
  case msg of
    Nothing -> sendUiEvent $ ErrorEvent "Failed to decode Chat"
    Just m -> sendUiEvent $ ChatEvent m

readSendLoop :: Client m => m ()
readSendLoop = do
  msg <- getUserLine
  sendMessage $ CM.ChatMessage msg
  readSendLoop

requestUsernameHandler :: Client m => m ()
requestUsernameHandler = do
  sendUiEvent $ SystemEvent "Please enter your name"
  username <- getUserLine
  sendMessage $ CM.RequestUsername username
  msg <- recieveMessage
  case msg of
    Nothing ->
      sendUiEvent $ ErrorEvent "Failed to decode RequestUsernameResponse"
    Just SM.UsernameAccepted ->
      sendUiEvent $ SystemEvent $ "Username accepted, welcome to the chat " <> username <> "!"
    Just SM.UsernameAlreadyTaken -> do
      sendUiEvent $ SystemEvent "Name already taken, please try again"
      requestUsernameHandler

connectedHandler :: Client m => m ()
connectedHandler = do
  msg <- recieveMessage
  case msg of
    Nothing ->
      sendUiEvent $ SystemEvent "Failed to decode user connected: "
    Just SM.RequestUsername ->
      requestUsernameHandler
