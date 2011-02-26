Chloe = function (options) {
  options = options || {};
  options.host = options.host || 'localhost';
  options.port = options.port || 8888;

  this.transport = new Chloe.WebSocketTransport(options);
};

Chloe.prototype = {
  connect: function (callback) {
    this.transport.connect(callback);
  },
  onmessage: function (callback) {
    this.transport.onmessage(callback);
  },
  send: function (message) {
    this.transport.send(message);
  }
};
