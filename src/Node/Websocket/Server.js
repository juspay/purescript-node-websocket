"use strict";

var WSServer = require("websocket").server;

exports.newWebsocketServer = function (config) {
  return function () {
    return new WSServer(config);
  }
}

exports.onRequest = function (server) {
  return function (callback) {
    return function () {
      server.on("request", function (req) {
        callback(req)();
      })
    }
  }
}

exports.onConnect = function (server) {
  return function (callback) {
    return function () {
      server.on("connect", function (conn) {
        callback(conn)();
      })
    }
  }
}

exports.onClose = function (server) {
  return function (callback) {
    return function () {
      server.on("close", function (conn, reason, description) {
        callback(conn)(reason)(description)();
      })
    }
  }
}