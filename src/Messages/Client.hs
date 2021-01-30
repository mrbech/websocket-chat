{-# LANGUAGE DeriveGeneric #-}

module Messages.Client (
    RequestUsername(..),
    ChatMessage(..),
    encodeRequestUsername,
    decodeRequestUsername,
    encodeChatMessage,
    decodeChatMessage
) where

import qualified Data.Aeson as J
import GHC.Generics
import Data.Text (Text)
import Messages.Utils (decode'', encode'')

-- | Client Message Data Types
-- Following are the data types for messages that the client can send to the server.

-- Client requests the username as their username, is in response to
-- 'Messages.Server.RequestUsername'
newtype RequestUsername = RequestUsername { username :: Text } 
    deriving (Generic, Show)

encodeRequestUsername :: RequestUsername -> Text
encodeRequestUsername = encode''
decodeRequestUsername :: Text -> Maybe RequestUsername
decodeRequestUsername = decode''

-- Client sends a message to the chat.
newtype ChatMessage = ChatMessage { message :: Text }
    deriving (Generic, Show)

encodeChatMessage :: ChatMessage -> Text
encodeChatMessage = encode''
decodeChatMessage :: Text -> Maybe ChatMessage
decodeChatMessage = decode''

instance J.FromJSON RequestUsername
instance J.ToJSON RequestUsername where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON ChatMessage
instance J.ToJSON ChatMessage where
    toEncoding = J.genericToEncoding J.defaultOptions
