module Node.Websocket.Types where

import Control.Monad.Eff (kind Effect)
import Node.Buffer (Buffer)
import Node.HTTP (Server)

foreign import data WSSERVER :: Effect

foreign import data WSServer :: Type

foreign import data WSRequest :: Type

foreign import data WSConnection :: Type

newtype CloseReason = CloseReason Int

newtype CloseDescription = CloseDescription String

type ServerConfig =
  { httpServer :: Server
  , maxReceivedFrameSize :: Int
  , maxReceivedMessageSize :: Int
  , fragmentOutgoingMessages :: Boolean
  , fragmentationThreshold :: Int
  , keepalive :: Boolean
  , keepaliveInterval :: Int
  , dropConnectionOnKeepaliveTimeout :: Boolean
  , keepaliveGracePeriod :: Int
  , assembleFragments :: Boolean
  , autoAcceptConnections :: Boolean
  , useNativeKeepalive :: Boolean
  , closeTimeout :: Int
  , disableNagleAlgorithm :: Boolean
  , ignoreXForwardedFor :: Boolean
  }

defaultServerConfig :: Server -> ServerConfig
defaultServerConfig httpServer =
  { httpServer
  , maxReceivedFrameSize: 0x10000
  , maxReceivedMessageSize: 0x100000
  , fragmentOutgoingMessages: true
  , fragmentationThreshold: 0x4000
  , keepalive: true
  , keepaliveInterval: 20000
  , dropConnectionOnKeepaliveTimeout: true
  , keepaliveGracePeriod: 10000
  , assembleFragments: true
  , autoAcceptConnections: false
  , useNativeKeepalive: false
  , closeTimeout: 5000
  , disableNagleAlgorithm: true
  , ignoreXForwardedFor: false
  }

type Frame r = { type :: String | r }

newtype TextFrame = TextFrame (Frame (utf8Data :: String))

newtype BinaryFrame = BinaryFrame (Frame (binaryData :: Buffer))