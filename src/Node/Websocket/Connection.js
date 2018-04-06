"use strict";

exports.closeDescription = function (conn) {
  return conn.closeDescription;
}

exports.closeReasonCode = function (conn) {
  return conn.closeReasonCode;
}

exports.protocol = function (conn) {
  return conn.protocol;
}

exports.remoteAddress = function (conn) {
  return conn.remoteAddress;
}

exports.webSocketVersion = function (conn) {
  return conn.webSocketVersion;
}

exports.connected = function (conn) {
  return conn.connected;
}

exports.closeWithReason = function (conn) {
  return function (reasonCode) {
    return function (description) {
      return function () {
        conn.close(reasonCode, description);
      }
    }
  }
}

exports.close = function (conn) {
  return function () {
    conn.close();
  }
}

exports.drop = function (conn) {
  return function (reasonCode) {
    return function (description) {
      return function () {
        conn.drop(reasonCode, description);
      }
    }
  }
}

exports.sendUTF = function (conn) {
  return function (msg) {
    return function () {
      conn.sendUTF(msg);
    }
  }
}

exports.sendBytes = function (conn) {
  return function (buffer) {
    return function () {
      conn.sendBytes(buffer);
    }
  }
}

exports.ping = function (conn) {
  return function (buffer) {
    return function () {
      conn.ping(buffer);
    }
  }
}

exports.pong = function (conn) {
  return function (buffer) {
    return function () {
      conn.pong(buffer);
    }
  }
}

exports.sendFrame = function (conn) {
  return function (frame) {
    return function () {
      conn.sendFrame(frame);
    }
  }
}

exports.onMessageImpl = function (Left) {
  return function (Right) {
    return function (conn) {
      return function (callback) {
        return function () {
          conn.on("message", function (msg) {
            if (msg.type == "utf8") {
              callback(Left(msg))();
              return;
            }
            callback(Right(msg))();
          })
        }
      }
    }
  }
}

exports.onFrame = function (conn) {
  return function (callback) {
    return function () {
      conn.on("frame", function (frame) {
        callback(frame)();
      })
    }
  }
}

exports.onClose = function (conn) {
  return function (callback) {
    return function () {
      conn.on("close", function(reasonCode, description) {
        callback(reasonCode)(description)();
      })
    }
  }
}

exports.onError = function (conn) {
  return function (callback) {
    return function () {
      conn.on("error", function (err) {
        callback(err)();
      })
    }
  }
}

exports.onPing = function (conn) {
  return function (callback) {
    return function () {
      conn.on("ping", function(cancel, data) {
        callback(data)(cancel)();
      })
    }
  }
}

exports.onPong = function (conn) {
  return function (callback) {
    return function () {
      conn.on("pong", function (data) {
        callback(data)();
      })
    }
  }
}