Chloe.XhrTransport = function (options) {
  this.host = options.host;
  this.port = options.port;
  this.protocol = "http://";
  this.callbacks = {};
};

Chloe.XhrTransport.isEnabled = function (host) {
  return 'XMLHttpRequest' in window &&
         this.prototype.makeXhr().withCredentials != undefined;
};

Chloe.XhrTransport.prototype = {
  makeXhr: function () {
    return new XMLHttpRequest();
  },

  url: function (path) {
    return this.protocol + this.host + ":" + this.port + "/chloe" + path;
  },

  connect: function (callback) {
    var self = this,
        message = new Chloe.Message({
          type: 'connect'
        });

    message.pack();
    this.postRequest(message.packed, function (data) {
       self.sessionId = data.sessionId;
       self.listenForMessages();
       callback(message);
    });
  },

  send: function (outbound) {
    // TODO (mat): definitely need to move pack/unpack into the transport
    var message = Chloe.Message.unpack(outbound);
    message.sessionId = this.sessionId;
    message.pack();
    this.postRequest(message.packed);
  },

  onclose: function (callback) {
    this.callbacks.onclose = function () {
      clearTimeout(this.poller);
      callback();
    };
  },

  onmessage: function (callback) {
    this.callbacks.onmessage = callback;
  },

  noop: function () {
  },

  handleStateChange: function (req, callback) {
    var received = callback || this.noop,
        closed   = this.callbacks.onclose || this.noop;
    req.onreadystatechange = function(){
      var message, status;
      if (req.readyState == 4){
        req.onreadystatechange = this.noop;
        try { status = req.status; } catch(e){}
        if (status == 200){
          if (req.responseText !== "") {
            var data = JSON.parse(req.responseText);
            if (data.messages) {
              var messages = data.messages;
              for (var i in messages) {
                received(new Chloe.Message(messages[i]));
              }
            } else {
              received(new Chloe.Message(data));
            }
          }
        } else {
          closed();
        }
      }
    }
  },

  getRequest: function (callback) {
    var req = this.makeXhr(),
        message = new Chloe.Message({ sessionId: this.sessionId,
                                      type: "poll" });
    message.pack();
    req.open('GET', this.url("/xhr/" + (+ new Date)) +
                    "?data=" + escape(message.packed));
    this.handleStateChange(req, callback);
    req.send(null);
  },

  postRequest: function (data, callback) {
    var req = this.makeXhr();
    req.open('POST', this.url('/xhr'));
    if ('setRequestHeader' in req) {
      req.setRequestHeader('Content-type', 'application/x-www-form-urlencoded; charset=utf-8');
    }
    this.handleStateChange(req, callback);
    req.send("data=" + escape(data));
  },

  listenForMessages: function () {
    var self = this,
        onmessage = this.callbacks.onmessage || this.noop;
        message = new Chloe.Message({ sessionId: this.sessionId,
                                      type: "poll" });

    this.getRequest(function (incoming) {
      if (typeof(incoming) !== "undefined") {
        incoming.pack();
        onmessage(incoming.packed);
      }
      // XXX (trotter): The following works for fetching messages,
      //                but it causes a weird '<System>' error message
      //                to show up in firebug.
      self.listenForMessages();
    });
  }
};
