{-# LANGUAGE DeriveGeneric #-}

module Messages.Server
  ( UserConnected (..),
    RequestUsernameResponse (..),
    Chat (..),
    Message (..),
    UserJoined (..),
    UserLeft (..),
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import GHC.Generics

-- | Server Message Data Types
-- Following are the data types for messages that the server can send to a client.

-- Messages that server sends when the user connects
data UserConnected = RequestUsername deriving (Generic, Show)

data RequestUsernameResponse = UsernameAccepted | UsernameAlreadyTaken
  deriving (Generic, Show)

-- Possible messages sent to clients during chat
data Chat
  = ChatMessage Message
  | Sync [Text]
  | ChatUserJoined UserJoined
  | ChatUserLeft UserLeft
  deriving (Generic, Show)

data Message = Message {fromUsername :: Text, message :: Text}
  deriving (Generic, Show)

newtype UserJoined = UserJoined {usernameJoined :: Text}
  deriving (Generic, Show)

newtype UserLeft = UserLeft {usernameLeft :: Text}
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
