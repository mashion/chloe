Chloe = function (options) {
  options = options || {};
  options.host = options.host || 'localhost';
  options.port = options.port || 8888;

  this.transport = new Chloe.WebSocketTransport(options);
};

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
  }
};
