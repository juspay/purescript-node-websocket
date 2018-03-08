module Node.Websocket where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Node.Buffer (Buffer)
import Node.Websocket.Connection as Conn
import Node.Websocket.Request as Req
import Node.Websocket.Server as Server
import Node.Websocket.Types (BinaryFrame, CloseDescription, CloseReason, TextFrame, WSConnection, WSRequest, WSSERVER, WSServer)

class On (evt :: Event) obj callback out | evt -> callback obj out where
  on :: forall proxy. proxy evt -> obj -> callback -> out

foreign import kind Event

data EventProxy (e :: Event) = EventProxy

foreign import data ConnectionMessage :: Event

foreign import data ConnectionClose :: Event

foreign import data ConnectionError :: Event

foreign import data ConnectionPing :: Event

foreign import data ConnectionPong :: Event

foreign import data RequestAccepted :: Event

foreign import data RequestRejected :: Event

foreign import data Request :: Event

foreign import data Connect :: Event

foreign import data Close :: Event

type WSSEff e = (wss :: WSSERVER | e)

instance connectionOnMessage
  :: On ConnectionMessage WSConnection (Either TextFrame BinaryFrame -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Conn.onMessage

instance connectionOnClose
  :: On ConnectionClose WSConnection (CloseReason -> CloseDescription -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Conn.onClose

instance connectionOnError
  :: On ConnectionError WSConnection (Error -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Conn.onError

instance connectionOnPing
  :: On ConnectionPing WSConnection (Buffer -> Eff (wss :: WSSERVER | e) Unit -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Conn.onPing

instance connectionOnPong
  :: On ConnectionPong WSConnection (Buffer -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Conn.onPong

instance requestOnAccepted
  :: On RequestAccepted WSRequest (WSConnection -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Req.onRequestAccepted

instance requestOnRejected
  :: On RequestRejected WSRequest (Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Req.onRequestRejected

instance serverOnRequest
  :: On Request WSServer (WSRequest -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Server.onRequest

instance serverOnConnect
  :: On Connect WSServer (WSConnection -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Server.onConnect

instance serverOnClose
  :: On Close WSServer (WSConnection -> CloseReason -> CloseDescription -> Eff (wss :: WSSERVER | e) Unit) (Eff (wss :: WSSERVER | e) Unit)
  where
    on _ = Server.onClose