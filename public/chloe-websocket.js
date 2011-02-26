Chloe.WebSocketTransport = function (options) {
  this.host = options.host;
  this.port = options.port;
  this.socketAttributes = {};
};

Chloe.WebSocketTransport.prototype = {
  // Public API
  connect: function (callback) {
    var self = this;
    this.socket = new WebSocket("ws://" + this.host + ":" + this.port + "/chloe/websocket");
    this.socket.onopen = callback;
    for (var i in this.socketAttributes) {
      this.socket[i] = this.socketAttributes[i];
    }
  },
  onclose: function (callback) {
    this.attachToSocket('onclose', callback);
  },
  onmessage: function (callback) {
    this.attachToSocket('onmessage', function (message) { callback(message.data) });
  },
  send: function (message) {
    this.socket.send(message);
  },

  // Internal helpers
  attachToSocket: function (attribute, callback) {
    this.socketAttributes[attribute] = callback;
    if (this.socket) {
      this.socket[attribute] = this.socketAttributes[attribute];
    }
  }
};
