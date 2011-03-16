Chloe.Transport.WebSocket = function (options) {
  Chloe.Transport.Base.mixin(this);
  this.init(options);
  this.protocol = "ws";
  this.onclose = function (callback) {
    this.attachToSocket('onclose', callback);
  };
  this.onmessage = function (callback) {
    this.attachToSocket('onmessage', function (message) {
      callback(message.data)
    });
  };
};

Chloe.Transport.WebSocket.prototype = {
  // Public API
  connect: function (callback) {
    var self = this;
    this.socket = new WebSocket(this.url("/websocket"));
    this.socket.onopen = callback;
    for (var i in this.callbacks) {
      this.socket[i] = this.callbacks[i];
    }
  },
  send: function (message) {
    this.socket.send(message);
  },

  // Internal helpers
  attachToSocket: function (attribute, callback) {
    this.callbacks[attribute] = callback;
    if (this.socket) {
      this.socket[attribute] = this.socketAttributes[attribute];
    }
  }
};
