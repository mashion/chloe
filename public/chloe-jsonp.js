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
      self.callbacks.onconnect = null;
      self.handleConnect(data);
      callback();
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

  // Internal functions
  send: function (data) {
    var self = this;
    script = document.createElement('script');
    script.src = this.url(data);
    script.type = 'text/javascript';
    script.onerror = function () {
      // TODO: Find out what, if any, arguments this takes
      self.handleError();
    }
    document.body.appendChild(script);
  },

  register: function () {
    this.id   = (new Date()).getTime();
    Chloe.JsonpTransport.connections[this.id] = this;
  },

  url: function (data) {
    return this.protocol + this.host + ":" + this.port + "/chloe/jsonp.js?data=" + escape(data);
  },

  handleConnect: function (data) {
    this.sessionId = data.sessionId;
    console.log("We're connected with data: ");
    console.dir(data);
  },

  handleError: function () {
    console.log("oh noes, an error happened");
  }
};

Chloe.JsonpTransport.connections = {};

Chloe.JsonpTransport.response = function (data) {
  var message    = new Chloe.Message(data),
      connection = Chloe.JsonpTransport.connections[message.id];

  if (message.type == 'connect') {
    connection.callbacks.onconnect(message);
  } else {
    throw new Error("Unknown message type for JsonpTransport.");
  }
};
