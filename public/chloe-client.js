Chloe = function (options) {
  options = options || {};
  options.host = options.host || 'localhost';
  options.port = options.port || 8888;
  this.transport = this.makeTransport(options);
};

Chloe.Version = '0.0.1';
Chloe.Transport = {};

// Base utility functions for Chloe Transports
Chloe.Transport.Base = {
  url: function (path) {
    return this.protocol + "://" + this.host + ":" + this.port + "/chloe" + path;
  },
  mixin: function (child) {
    for (var i in this) {
      child[i] = this[i];
    }
  }
}

Chloe.prototype = {
  // Public API
  connect: function (callback) {
    this.transport.connect(callback);
  },
  onmessage: function (callback) {
    var self = this;
    this.onmessageCallback = callback;
    this.transport.onmessage(function (message) {
      self.handleMessage(Chloe.Message.unpack(message));
    });
  },
  onclose: function (callback) {
    this.transport.onclose(callback);
  },
  send: function (data) {
    var message = Chloe.Message.pack(data);
    this.transport.send(message.packed);
  },

  // Internal functions
  handleMessage: function (message) {
    this.onmessageCallback(message.data);
  },
  makeTransport: function (options) {
    var Transport = Chloe.Transport[options.transport] || Chloe.Transport.WebSocket;
    return new Transport(options);
  }
};
