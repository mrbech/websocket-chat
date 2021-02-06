{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.Lib where

import qualified Messages.Server as SM
import qualified Messages.Client as CM
import Client.Core (Client(..))

listenForMessage :: Client m => m ()
listenForMessage = do
    msg <- recieveMessage
    case msg of
        Nothing -> sendUserLine "Failed to decode Chat"
        Just (SM.ChatMessage SM.Message { SM.fromUsername, SM.message }) -> 
            sendUserLine (fromUsername <> ": " <> message)
        Just (SM.ChatUserJoined SM.UserJoined { SM.usernameJoined }) -> 
            sendUserLine (usernameJoined <> " joined")
        Just (SM.ChatUserLeft SM.UserLeft { SM.usernameLeft }) -> 
            sendUserLine (usernameLeft <> " left")

readSendLoop :: Client m => m ()
readSendLoop = do
    msg <- getUserLine
    if msg == ":quit" then
        return ()
    else do
        sendMessage $ CM.ChatMessage msg
        readSendLoop

requestUsernameHandler :: Client m => m ()
requestUsernameHandler = do
    sendUserLine "Please enter your name:"
    username <- getUserLine
    sendMessage $ CM.RequestUsername username
    msg <- recieveMessage
    case msg of
        Nothing ->
            sendUserLine "Failed to decode RequestUsernameResponse"
        Just SM.UsernameAccepted -> 
            sendUserLine "Username accepted, welcome to the chat!"
        Just SM.UsernameAlreadyTaken -> do
            sendUserLine "Name already taken, please try again"
            requestUsernameHandler

connectedHandler :: Client m => m ()
connectedHandler = do
    msg <- recieveMessage
    case  msg of
        Nothing -> 
            sendUserLine "Failed to decode user connected: "
        Just SM.RequestUsername ->
            requestUsernameHandler
