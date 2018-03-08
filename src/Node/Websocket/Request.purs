module Node.Websocket.Request where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Nullable (Nullable)
import Node.HTTP.Client (Request)
import Node.URL (URL)
import Node.Websocket.Types (WSConnection, WSRequest, WSSERVER)

foreign import httpRequest :: WSRequest -> Request

foreign import host :: WSRequest -> String

foreign import resource :: WSRequest -> String

foreign import resourceURL :: WSRequest -> URL

foreign import remoteAddress :: WSRequest -> String

foreign import webSocketVersion :: WSRequest -> Number

foreign import origin :: WSRequest -> Nullable String

foreign import requestedProtocols :: WSRequest -> Array String

foreign import accept :: forall e. WSRequest -> Nullable String -> Nullable String -> Eff (wss :: WSSERVER | e) WSConnection

foreign import reject :: forall e. WSRequest -> Nullable Int -> Nullable String -> Eff (wss :: WSSERVER | e) Unit

type RequestAcceptedCallback e = WSConnection -> Eff (wss :: WSSERVER | e) Unit

foreign import onRequestAccepted :: forall e. WSRequest -> RequestAcceptedCallback e -> Eff (wss :: WSSERVER | e) Unit

type RequestRejectedCallback e = Eff (wss :: WSSERVER | e) Unit

foreign import onRequestRejected :: forall e. WSRequest -> RequestRejectedCallback e -> Eff (wss :: WSSERVER | e) Unit