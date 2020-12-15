module Server.Client where
import Data.Text
import qualified Network.WebSockets as WS

newtype Username = Username Text deriving (Show, Eq, Ord)

data Client = Client {
    username :: Username,
    connection :: WS.Connection
}

usernameText :: Client -> Text
usernameText = (\(Username t) -> t) . username
