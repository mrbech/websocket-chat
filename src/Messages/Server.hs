{-# LANGUAGE DeriveGeneric #-}

module Messages.Server (
    UserConnected(..),
    RequestUsernameResponse(..),
    Chat(..),
    Message(..),
    UserJoined(..),
    UserLeft(..),
    encodeUserConnected,
    decodeUserConnected,
    encodeRequestUsernameResponse,
    decodeRequestUsernameResponse,
    encodeChat,
    decodeChat
) where

import qualified Data.Aeson as J
import GHC.Generics
import Data.Text (Text)
import Messages.Utils (decode'', encode'')

-- | Server Message Data Types
-- Following are the data types for messages that the server can send to a client.

-- Messages that server sends when the user connects
data UserConnected = RequestUsername deriving (Generic, Show)

encodeUserConnected :: UserConnected -> Text
encodeUserConnected = encode''
decodeUserConnected :: Text -> Maybe UserConnected
decodeUserConnected = decode''

data RequestUsernameResponse = UsernameAccepted | UsernameAlreadyTaken
    deriving (Generic, Show)

encodeRequestUsernameResponse :: RequestUsernameResponse -> Text
encodeRequestUsernameResponse = encode''
decodeRequestUsernameResponse :: Text -> Maybe RequestUsernameResponse
decodeRequestUsernameResponse = decode''

-- Possible messages sent to clients during chat
data Chat = 
    ChatMessage Message
    | ChatUserJoined UserJoined
    | ChatUserLeft UserLeft
    deriving (Generic, Show)

encodeChat :: Chat -> Text
encodeChat = encode''
decodeChat :: Text -> Maybe Chat
decodeChat = decode''

data Message = Message { fromUsername :: Text, message :: Text }
    deriving (Generic, Show)
newtype UserJoined = UserJoined { usernameJoined :: Text }
    deriving (Generic, Show)
newtype UserLeft = UserLeft { usernameLeft :: Text }
    deriving (Generic, Show)

-- JSON boilerplate
instance J.FromJSON UserConnected
instance J.ToJSON UserConnected where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON RequestUsernameResponse 
instance J.ToJSON RequestUsernameResponse where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Chat
instance J.ToJSON Chat where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Message
instance J.ToJSON Message where
    toEncoding = J.genericToEncoding J.defaultOptions
instance J.FromJSON UserJoined
instance J.ToJSON UserJoined where
    toEncoding = J.genericToEncoding J.defaultOptions
instance J.FromJSON UserLeft
instance J.ToJSON UserLeft where
    toEncoding = J.genericToEncoding J.defaultOptions
