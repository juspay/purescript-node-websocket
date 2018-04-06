module Node.Websocket.Connection
  ( closeDescription
  , closeReasonCode
  , protocol
  , remoteAddress
  , webSocketVersion
  , connected
  , closeWithReason
  , close
  , drop
  , sendUTF
  , sendBytes
  , sendMessage
  , ping
  , pong
  , sendFrame
  , MessageCallback
  , onMessage
  , FrameCallback
  , onFrame
  , ErrorCallback
  , onError
  , CloseCallback
  , onClose
  , PingCallback
  , onPing
  , PongCallback
  , onPong
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Nullable (Nullable)
import Node.Buffer (Buffer)
import Node.Websocket.Types (BinaryFrame(..), CloseDescription, CloseReason, TextFrame(..), WSConnection, WSFrame, WSSERVER)

foreign import closeDescription :: WSConnection -> Nullable CloseDescription

foreign import closeReasonCode :: WSConnection -> CloseReason

-- TODO: implement socket
-- Problem: there's no bindings to node's net module

foreign import protocol :: WSConnection -> String

foreign import remoteAddress :: WSConnection -> String

foreign import webSocketVersion :: WSConnection -> Number

foreign import connected :: WSConnection -> Boolean

foreign import closeWithReason :: forall e. WSConnection -> CloseReason -> CloseDescription -> Eff (wss :: WSSERVER | e) Unit

foreign import close :: forall e. WSConnection -> Eff (wss :: WSSERVER | e) Unit

-- | See https://github.com/theturtle32/WebSocket-Node/blob/master/docs/WebSocketConnection.md#dropreasoncode-description
foreign import drop :: forall e. WSConnection -> CloseReason -> CloseDescription -> Eff (wss :: WSSERVER | e) Unit

foreign import sendUTF :: forall e. WSConnection -> String -> Eff (wss :: WSSERVER | e) Unit

foreign import sendBytes :: forall e. WSConnection -> Buffer -> Eff (wss :: WSSERVER | e) Unit

sendMessage :: forall e. WSConnection -> Either TextFrame BinaryFrame -> Eff (wss :: WSSERVER | e) Unit
sendMessage conn = case _ of
  Left (TextFrame msg) -> sendUTF conn msg.utf8Data
  Right (BinaryFrame msg) -> sendBytes conn msg.binaryData

foreign import ping :: forall e. WSConnection -> Buffer -> Eff (wss :: WSSERVER | e) Unit

foreign import pong :: forall e. WSConnection -> Buffer -> Eff (wss :: WSSERVER | e) Unit

foreign import sendFrame :: forall e. WSConnection -> WSFrame -> Eff (wss :: WSSERVER | e) Unit

type MessageCallback e = Either TextFrame BinaryFrame -> Eff (wss :: WSSERVER | e) Unit

foreign import onMessageImpl :: forall a b e. (a -> Either a b) -> (b -> Either a b) -> WSConnection -> MessageCallback e -> Eff (wss :: WSSERVER | e) Unit

onMessage :: forall e. WSConnection -> MessageCallback e -> Eff (wss :: WSSERVER | e) Unit
onMessage = onMessageImpl Left Right

type FrameCallback e = WSFrame -> Eff (wss :: WSSERVER | e) Unit

foreign import onFrame :: forall e. WSConnection -> FrameCallback e -> Eff (wss :: WSSERVER | e) Unit

type CloseCallback e = CloseReason -> CloseDescription -> Eff (wss :: WSSERVER | e) Unit

foreign import onClose :: forall e. WSConnection -> CloseCallback e -> Eff (wss :: WSSERVER | e) Unit

type ErrorCallback e = Error -> Eff (wss :: WSSERVER | e) Unit

foreign import onError :: forall e. WSConnection -> ErrorCallback e -> Eff (wss :: WSSERVER | e) Unit

type PingCallback e = Buffer -> Eff (wss :: WSSERVER | e) Unit -> Eff (wss :: WSSERVER | e) Unit

foreign import onPing :: forall e. WSConnection -> PingCallback e -> Eff (wss :: WSSERVER | e) Unit

type PongCallback e = Buffer -> Eff (wss :: WSSERVER | e) Unit

foreign import onPong :: forall e. WSConnection -> PongCallback e -> Eff (wss :: WSSERVER | e) Unit