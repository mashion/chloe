Chloe.Transport.WebSocket = function (options) {
  Chloe.Transport.Base.mixin(this);
  this.host = options.host;
  this.port = options.port;
  this.protocol = "ws";
  this.socketAttributes = {};
};

Chloe.Transport.WebSocket.prototype = {
  // Public API
  connect: function (callback) {
    var self = this;
    this.socket = new WebSocket(this.url("/websocket"));
    this.socket.onopen = callback;
    for (var i in this.socketAttributes) {
      this.socket[i] = this.socketAttributes[i];
    }
  },
  onclose: function (callback) {
    this.attachToSocket('onclose', callback);
  },
  onmessage: function (callback) {
    this.attachToSocket('onmessage', function (message) {
      callback(message.data)
    });
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
