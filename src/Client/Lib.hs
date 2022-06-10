{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Lib where

import Client.Core (Client (..), UiEvent (..))
import qualified Data.Text as Text
import qualified Messages.Client as CM
import qualified Messages.Server as SM

listenForMessage :: Client m => m Bool
listenForMessage = do
  msg <- recieveMessage
  case msg of
    Left s -> do
      sendUiEvent $ ErrorEvent (Text.pack $ s <> "\nPlease try restarting client")
      return False
    Right m -> do
      sendUiEvent $ ChatEvent m
      return True

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
    Left s ->
      sendUiEvent $ ErrorEvent (Text.pack $ s <> "\nPlease try restarting client")
    Right SM.UsernameAccepted ->
      sendUiEvent $ SystemEvent $ "Username accepted, welcome to the chat " <> username <> "!"
    Right SM.UsernameAlreadyTaken -> do
      sendUiEvent $ SystemEvent "Name already taken, please try again"
      requestUsernameHandler

connectedHandler :: Client m => m ()
connectedHandler = do
  msg <- recieveMessage
  case msg of
    Left s ->
      sendUiEvent $ SystemEvent (Text.pack s)
    Right SM.RequestUsername ->
      requestUsernameHandler
