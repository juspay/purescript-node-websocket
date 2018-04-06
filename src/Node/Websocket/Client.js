"use strict";

var WSClient = require("websocket").client;

exports.newWebsocketClient = function (config) {
  return function () {
    return new WSClient(config);
  }
}

exports.connect = function (client) {
  return function (requestUrl) {
    return function (requestProtocols) {
      return function (origin) {
        return function (headers) {
          return function (requestOptions) {
            return function () {
              client.connect(requestUrl, requestProtocols, origin, headers, requestOptions);
            }
          }
        }
      }
    }
  }
}

exports.abort = function (client) {
  return function () {
    client.abort();
  }
}

exports.onConnect = function (client) {
  return function (callback) {
    return function () {
      client.on("connect", function (conn) {
        callback(conn)();
      })
    }
  }
}

exports.onConnectFailed = function (client) {
  return function (callback) {
    return function () {
      client.on("connectFailed", function (errorDescription) {
        callback(errorDescription)();
      })
    }
  }
}

exports.onHttpResponse = function (client) {
  return function (callback) {
    return function () {
      client.on("httpResponse", function (response, webSocketClient) {
        callback(response)(webSocketClient)();
      })
    }
  }
}