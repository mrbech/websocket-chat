module Server.ServerState where

import qualified Server.Client as Client
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

type ServerState = Map.Map Client.Username Client.Client

newServerState :: ServerState
newServerState = Map.empty

connections :: ServerState -> [WS.Connection]
connections = fmap Client.connection . Map.elems

addClient :: Client.Client -> ServerState -> ServerState
addClient client = Map.insert (Client.username client) client

removeClient :: Client.Client -> ServerState -> ServerState
removeClient client = Map.delete (Client.username client)

containsUsername :: Client.Username -> ServerState -> Bool
containsUsername = Map.member
