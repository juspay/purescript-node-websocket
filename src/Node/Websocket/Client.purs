module Node.Websocket.Client where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)
import Data.Nullable (Nullable)
import Node.HTTP as HTTP
import Node.Websocket.Types (ClientConfig, ErrorDescription, WSCLIENT, WSClient, WSConnection)

foreign import newWebsocketClient :: forall e. ClientConfig -> Eff (wsc :: WSCLIENT | e) WSClient

foreign import connect :: forall e. WSClient -> String -> Nullable (Array String) -> Nullable String -> Nullable Foreign -> Nullable Foreign -> Eff (wsc :: WSCLIENT | e) Unit

foreign import abort :: forall e. WSClient -> Eff (wsc :: WSCLIENT | e) Unit

type ConnectCallback e = WSConnection -> Eff (wsc :: WSCLIENT | e) Unit

foreign import onConnect :: forall e. WSClient -> ConnectCallback e -> Eff (wsc :: WSCLIENT | e) Unit

type ConnectFailedCallback e = ErrorDescription -> Eff (wsc :: WSCLIENT | e) Unit

foreign import onConnectFailed :: forall e. WSClient -> ConnectFailedCallback e -> Eff (wsc :: WSCLIENT | e) Unit

type HttpResponseCallback e =  HTTP.Response -> WSClient -> Eff (wsc :: WSCLIENT | e) Unit

foreign import onHttpResponse :: forall e. WSClient -> HttpResponseCallback e -> Eff (wsc :: WSCLIENT | e) Unit