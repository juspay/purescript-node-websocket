"use strict";

var WSFrame = require("websocket").frame;

exports.newWebsocketFrame = function () {
  return new WSFrame();
}

exports.unsafeGet = function (frame) {
  return function (prop) {
    return frame[prop];
  }
}

exports.unsafeSet = function (frame) {
  return function (prop) {
    return function (val) {
      return function () {
        frame[prop] = val;
      }
    }
  }
}