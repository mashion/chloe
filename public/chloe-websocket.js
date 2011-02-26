Chloe.WebSocketTransport = function (options) {
  this.host = options.host;
  this.port = options.port;
};

Chloe.WebSocketTransport.prototype = {
  connect: function (callback) {
    this.socket = new WebSocket("ws://" + this.host + ":" + this.port + "/socket.io/websocket");
    this.socket.onopen = callback;
    if (this.messageCallback) {
      this.socket.onmessage = this.messageCallback;
    }
  },
  onmessage: function (callback) {
    this.messageCallback = callback;
    if (this.socket) {
      this.socket.onmessage = this.messageCallback;
    }
  },
  send: function (message) {
    this.socket.send(message);
  }
};
