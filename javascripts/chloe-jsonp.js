Chloe.JsonpTransport = function (options) {
  this.host = options.host;
  this.port = options.port;
  this.protocol = "http://";
  this.callbacks = {};
  this.register();
};

Chloe.JsonpTransport.prototype = {
  // Public API
  connect: function (callback) {
    var self = this,
        message = new Chloe.Message({
          id: this.id,
          type: 'connect'
        });

    this.callbacks.onconnect = function (data) {
      // TODO: Storing sessionId both here and on client level.
      //       Feels marginally wrong.
      self.sessionId = data.sessionId;
      self.listenForMessages();
      callback(data);
    };

    message.pack();
    message.send(this);
  },

  onmessage: function (callback) {
    this.callbacks.onmessage = callback;
  },

  onclose: function (callback) {
    this.callbacks.onclose = callback;
  },

  send: function (data, options) {
    var self = this,
        script = document.createElement('script');

    options = options || {};

    script.src = this.url(data);
    script.type = 'text/javascript';
    script.onerror = function () {
      if (options.onerror) {
        options.onerror();
      }
      // TODO: Find out what, if any, arguments this takes
      self.handleError();
    };
    document.body.appendChild(script);
  },

  // Internal functions
  register: function () {
    this.id   = (new Date()).getTime();
    Chloe.JsonpTransport.connections[this.id] = this;
  },

  url: function (data) {
    return this.protocol + this.host + ":" + this.port + "/chloe/jsonp.js?data=" + escape(data) + "ts=" + (new Date()).getTime();
  },

  handleError: function () {
    // Need to figure out what to do here.
  },

  listenForMessages: function () {
    var self = this,
        message = new Chloe.Message({
                                      id: this.id,
                                      sessionId: this.sessionId,
                                      type: "poll"
                                    });
    message.send(this, {onerror: function () { self.listenForMessages(); }});
  }
};

Chloe.JsonpTransport.connections = {};

Chloe.JsonpTransport.isEnabled = function () {
  return true;
}

Chloe.JsonpTransport.response = function (data) {
  var message    = new Chloe.Message(data),
      connection = Chloe.JsonpTransport.connections[message.id];

  // TODO: We are packing because chloe-client.js is going to try to unpack
  // later. We need to remove this dependency and instead have the transports
  // take care of unpacking.
  message.pack();
  if (message.type === 'connect') {
    connection.callbacks.onconnect(message);
  } else if (message.type === 'poll') {
    connection.callbacks.onmessage(message.packed);
    connection.listenForMessages();
  } else {
    throw new Error("Unknown message type for JsonpTransport.");
  }
};
