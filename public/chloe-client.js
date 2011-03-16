Chloe = function (options) {
  options = options || {};
  options.host = options.host || 'localhost';
  options.port = options.port || 8901;
  this.transport = this.makeTransport(options);
  this.channelSubscriptions = {};
};

Chloe.Version = '0.0.1';
Chloe.Transport = {};

// Base utility functions for Chloe Transports
Chloe.Transport.Base = {
  url: function (path) {
    return this.protocol + "://" + this.host + ":" + this.port + "/chloe" + path;
  },
  init: function (options) {
    this.host = options.host;
    this.port = options.port;
    this.callbacks = {};
    this.protocol = "http";
  },
  onmessage: function (callback) {
    this.callbacks.onmessage = callback;
  },
  onclose: function (callback) {
    this.callbacks.onclose = callback;
  },
  mixin: function (child) {
    // TODO (mat): try doing a prototype chain here instead
    for (var i in this) {
      child[i] = this[i];
    }
  }
}

Chloe.prototype = {
  // Public API
  connect: function (callback) {
    self = this;
    this.transport.connect(function (data) {
      self.sessionId = data.sessionId;
      callback();
    });
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
    var message = Chloe.Message.pack(data, this.sessionId);
    message.send(this.transport);
  },
  subscribe: function (channel, callback) {
    var message = Chloe.Message.channelSubscribe(channel);
    this.channelSubscriptions[channel] = callback;
    message.send(this.transport);
  },

  // Internal functions
  handleMessage: function (message) {
    var callback = this.channelSubscriptions[message.channel];
    if (callback) {
      callback(message.data);
    } else {
      this.onmessageCallback(message.data);
    }
  },
  makeTransport: function (options) {
    var Transport = Chloe.Transport[options.transport] || Chloe.Transport.WebSocket;
    return new Transport(options);
  }
};
