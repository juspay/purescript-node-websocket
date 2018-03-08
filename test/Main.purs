module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Node.HTTP (HTTP, listen)
import Node.HTTP as HTTP
import Node.Websocket (ConnectionClose, ConnectionMessage, EventProxy(EventProxy), on)
import Node.Websocket.Connection (remoteAddress, sendBytes, sendUTF)
import Node.Websocket.Request (accept, origin)
import Node.Websocket.Server (newWebsocketServer, onRequest)
import Node.Websocket.Types (BinaryFrame(..), TextFrame(..), WSSERVER, defaultServerConfig)

main :: forall e. Eff (wss :: WSSERVER, console :: CONSOLE, http :: HTTP | e) Unit
main = do
  httpServer <- HTTP.createServer \ _ _ -> log "Server created"
  listen httpServer {hostname: "localhost", port: 8080, backlog: Nothing} (pure unit)
  wsServer <- newWebsocketServer (defaultServerConfig httpServer)
  onRequest wsServer \ req -> do
    log "New request received:"
    logShow (origin req)
    conn <- accept req (toNullable Nothing) (origin req)
    on message conn \ msg -> case msg of
      Left (TextFrame utf8msg) -> do
        log "Received utf8 message:"
        log utf8msg.utf8Data
        sendUTF conn utf8msg.utf8Data
      Right (BinaryFrame binaryMsg) -> do
        log "Received binary message"
        sendBytes conn binaryMsg.binaryData
    on close conn \ _ _ -> do
      log "Connection closed from:"
      log (remoteAddress conn)
  where
    close :: EventProxy ConnectionClose
    close = EventProxy
    message :: EventProxy ConnectionMessage
    message = EventProxy