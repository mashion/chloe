Chloe = function (options) {
  options = options || {};
  options.host = options.host || 'localhost';
  options.port = options.port || 8901;
  options.transport = options.transport || 'websocket';

  var transports = {
    websocket: Chloe.WebSocketTransport,
    jsonp:     Chloe.JsonpTransport
  }

  this.transport = new transports[options.transport](options);
  this.channelSubscriptions = {};
};

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
    var message = Chloe.Message.pack(data);
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
  }
};
