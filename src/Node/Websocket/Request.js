"use strict";

exports.httpRequest = function (req) {
  return req.httpRequest;
}

exports.host = function (req) {
  return req.host;
}

exports.resource = function (req) {
  return req.resource;
}

exports.resourceURL = function (req) {
  return req.resourceURL;
}

exports.remoteAddress = function (req) {
  return req.remoteAddress;
}

exports.webSocketVersion = function (req) {
  return req.webSocketVersion;
}

exports.origin = function (req) {
  return req.origin;
}

exports.requestedProtocols = function (req) {
  return req.requestedProtocols;
}

exports.accept = function (req) {
  return function (acceptedProtocol) {
    return function (allowedOrigin) {
      return function () {
        return req.accept(acceptedProtocol, allowedOrigin);
      }
    }
  }
}

exports.reject = function (req) {
  return function (httpStatus) {
    return function (reason) {
      return function () {
        req.reject(httpStatus, reason);
      }
    }
  }
}

exports.onRequestAccepted = function (req) {
  return function (callback) {
    return function () {
      req.on("requestAccepted", function(conn) {
        callback(conn)();
      });
    }
  }
}

exports.onRequestRejected = function (req) {
  return function (callback) {
    return function () {
      req.on("requestRejected", function() {
        callback();
      });
    }
  }
}