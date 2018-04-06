module Node.Websocket.Frame
  ( newWebsocketFrame
  , getFin, setFin
  , getRsv1, setRsv1
  , getRsv2, setRsv2
  , getRsv3, setRsv3
  , getMask, setMask
  , getOpCode, setOpCode
  , getLength
  , getBinaryPayload, setBinaryPayload
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Node.Buffer (Buffer)
import Node.Websocket.Types (OpCode(..), WSFrame, WSSERVER)

foreign import newWebsocketFrame :: forall e. Eff (wss :: WSSERVER | e) WSFrame

foreign import unsafeGet :: forall a. WSFrame -> String -> a

foreign import unsafeSet :: forall e a. WSFrame -> String -> a -> Eff (wss :: WSSERVER | e) Unit

getFin :: WSFrame -> Boolean
getFin = unsafeGet <@> "fin"

setFin :: forall e. WSFrame -> Boolean -> Eff (wss :: WSSERVER | e) Unit
setFin = unsafeSet <@> "fin"

getRsv1 :: WSFrame -> Boolean
getRsv1 = unsafeGet <@> "rsv1"

setRsv1 :: forall e. WSFrame -> Boolean -> Eff (wss :: WSSERVER | e) Unit
setRsv1 = unsafeSet <@> "rsv1"

getRsv2 :: WSFrame -> Boolean
getRsv2 = unsafeGet <@> "rsv2"

setRsv2 :: forall e. WSFrame -> Boolean -> Eff (wss :: WSSERVER | e) Unit
setRsv2 = unsafeSet <@> "rsv2"

getRsv3 :: WSFrame -> Boolean
getRsv3 = unsafeGet <@> "rsv3"

setRsv3 :: forall e. WSFrame -> Boolean -> Eff (wss :: WSSERVER | e) Unit
setRsv3 = unsafeSet <@> "rsv3"

getMask :: WSFrame -> Int
getMask = unsafeGet <@> "mask"

setMask :: forall e. WSFrame -> Int -> Eff (wss :: WSSERVER | e) Unit
setMask = unsafeSet <@> "mask"

getOpCode :: WSFrame -> OpCode
getOpCode f = case unsafeGet f "opcode" of
  0 -> Continuation
  1 -> Text
  2 -> Binary
  8 -> Close
  9 -> Ping
  _ -> Pong

setOpCode :: forall e. WSFrame -> OpCode -> Eff (wss :: WSSERVER | e) Unit
setOpCode f o = unsafeSet f "opcode" case o of
  Continuation -> 0
  Text -> 1
  Binary -> 2
  Close -> 8
  Ping -> 9
  Pong -> 10

getLength :: WSFrame -> Int
getLength = unsafeGet <@> "length"

getBinaryPayload :: WSFrame -> Buffer
getBinaryPayload = unsafeGet <@> "binaryPayload"

setBinaryPayload :: forall e. WSFrame -> Buffer -> Eff (wss :: WSSERVER | e) Unit
setBinaryPayload = unsafeSet <@> "binaryPayload"