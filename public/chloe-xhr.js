Chloe.Transport.XHR = function (options) {
  var self = this;
  Chloe.Transport.Base.mixin(this);
  this.init(options);
  this.detectXhrTechnique();
  this.onclose = function (callback) {
    self.callbacks.onclose = function () {
      clearTimeout(self.poller);
      callback();
    };
  };
};

Chloe.Transport.XHR.prototype = {
  // Transport functions
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

  // Internal functions
  detectXhrTechnique: function () {
    this.isCrossDomain = this.host !== document.domain;
    this.detectCORS();
  },
  detectCORS: function () {
    if (!('XMLHttpRequest' in window)) return false;
    var xhr = new XMLHttpRequest();
    this.supportsCORS = xhr.withCredentials != undefined;
  },
  makeXhr: function () {
    if ('XDomainRequest' in window && this.isCrossDomain) return new XDomainRequest();
    if ('XMLHttpRequest' in window && (!this.isCrossDomain || this.supportsCORS)) return new XMLHttpRequest();
    if (!this.isCrossDomain){
      try {
        var a = new ActiveXObject('MSXML2.XMLHTTP');
        return a;
      } catch(e){}

      try {
        var b = new ActiveXObject('Microsoft.XMLHTTP');
        return b;
      } catch(e){}
    }
    return false;
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
            message = Chloe.Message.unpack(req.responseText);
          }
          received(message);
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
        onmessage(incoming.packed);
      }
    });

    this.poller = setTimeout(function () {
      self.listenForMessages();
    }, 1000);
  }
};
