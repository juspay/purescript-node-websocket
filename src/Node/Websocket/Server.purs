module Node.Websocket.Server where

import Prelude

import Control.Monad.Eff (Eff)
import Node.Websocket.Types (CloseDescription, CloseReason, ServerConfig, WSConnection, WSRequest, WSSERVER, WSServer)

foreign import newWebsocketServer :: forall e. ServerConfig -> Eff (wss :: WSSERVER | e) WSServer

type RequestCallback e = WSRequest -> Eff (wss :: WSSERVER | e) Unit

foreign import onRequest :: forall e. WSServer -> RequestCallback e -> Eff (wss :: WSSERVER | e) Unit

type ConnectCallback e = WSConnection -> Eff (wss :: WSSERVER | e) Unit

foreign import onConnect :: forall e. WSServer -> ConnectCallback e -> Eff (wss :: WSSERVER | e) Unit

type CloseCallback e =
  WSConnection -> CloseReason -> CloseDescription -> Eff (wss :: WSSERVER | e) Unit

foreign import onClose :: forall e. WSServer -> CloseCallback e -> Eff (wss :: WSSERVER | e) Unit