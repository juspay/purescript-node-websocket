module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Data.Array as Array
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Node.HTTP (HTTP, listen)
import Node.HTTP as HTTP
import Node.Websocket (ConnectionClose, ConnectionMessage, EventProxy(EventProxy), Request, on)
import Node.Websocket.Connection (remoteAddress, sendMessage)
import Node.Websocket.Request (accept, origin)
import Node.Websocket.Server (newWebsocketServer)
import Node.Websocket.Types (WSSERVER, defaultServerConfig)

-- | Routes incoming messages to all clients except the one that sent it.
main :: forall e. Eff (ref :: REF, wss :: WSSERVER, console :: CONSOLE, http :: HTTP | e) Unit
main = do

  httpServer <- HTTP.createServer \ _ _ -> log "Server created"
  listen
    httpServer
    {hostname: "localhost", port: 2718, backlog: Nothing} do
      log "Server now listening"

  wsServer <- newWebsocketServer (defaultServerConfig httpServer)

  clientsRef <- newRef []

  on request wsServer \ req -> do
    log do
      "New connection from: " <> show (origin req)
    conn <- accept req (toNullable Nothing) (origin req)
    modifyRef clientsRef do
      Array.snoc <@> conn

    clients <- readRef clientsRef
    let idx = Array.length clients - 1

    log "New connection accepted"

    on message conn \ msg -> do

      forWithIndex_ clients \ i client -> do

        when (i /= idx) do
          sendMessage client msg

    on close conn \ _ _ -> do
      log do
        "Peer disconnected " <> remoteAddress conn
  where
    close = EventProxy :: EventProxy ConnectionClose
    message = EventProxy :: EventProxy ConnectionMessage
    request = EventProxy :: EventProxy Request