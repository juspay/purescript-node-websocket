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
import Type.Row.Effect.Equality (class EffectRowEquals, effFrom, effTo)

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
  :: (EffectRowEquals e (WSSEff e'))
  => On ConnectionMessage WSConnection (Either TextFrame BinaryFrame -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Conn.onMessage obj \ frame -> effTo (cb frame))

instance connectionOnClose
  :: EffectRowEquals e (WSSEff e')
  => On ConnectionClose WSConnection (CloseReason -> CloseDescription -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Conn.onClose obj \ cr cd -> effTo (cb cr cd))

instance connectionOnError
  :: EffectRowEquals e (WSSEff e')
  => On ConnectionError WSConnection (Error -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Conn.onError obj \ err -> effTo (cb err))

instance connectionOnPing
  :: EffectRowEquals e (WSSEff e')
  => On ConnectionPing WSConnection (Buffer -> Eff e Unit -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Conn.onPing obj \ buffer close -> effTo (cb buffer (effFrom close)))

instance connectionOnPong
  :: EffectRowEquals e (WSSEff e')
  => On ConnectionPong WSConnection (Buffer -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Conn.onPong obj \ buffer -> effTo (cb buffer))

instance requestOnAccepted
  :: EffectRowEquals e (WSSEff e')
  => On RequestAccepted WSRequest (WSConnection -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Req.onRequestAccepted obj \ conn -> effTo (cb conn))

instance requestOnRejected
  :: EffectRowEquals e (WSSEff e')
  => On RequestRejected WSRequest (Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Req.onRequestRejected obj (effTo cb ))

instance serverOnRequest
  :: EffectRowEquals e (WSSEff e')
  => On Request WSServer (WSRequest -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Server.onRequest obj \ req -> effTo (cb req))

instance serverOnConnect
  :: EffectRowEquals e (WSSEff e')
  => On Connect WSServer (WSConnection -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Server.onConnect obj \ conn -> effTo (cb conn))

instance serverOnClose
  :: EffectRowEquals e (WSSEff e')
  => On Close WSServer (WSConnection -> CloseReason -> CloseDescription -> Eff e Unit) (Eff e Unit)
  where
    on _ obj cb = effFrom (Server.onClose obj \ conn reasonCode description -> effTo (cb conn reasonCode description))