Chloe.Transport.XHR = function (options) {
  Chloe.Transport.Base.mixin(this);
  this.init(options);
};

Chloe.Transport.XHR.prototype = {
  // Transport functions
  connect: function (callback) {
    var message = new Chloe.Message({
          type: 'connect'
        });

    this.callbacks.onconnect = callback;
    this.detectXhrTechnique();

    message.pack();
    message.send(this);
  },

  send: function (message) {
    var self = this,
        url = this.makeUrl('POST');
    this.xhr = this.request('POST', url);
    this.xhr.onreadystatechange = function(){
      var status;
      if (self.xhr.readyState == 4){
        self.xhr.onreadystatechange = function() {};
        try { status = self.xhr.status; } catch(e){}
        if (status == 200){
          console.log(self.xhr.responseText);
        } else {
          console.log("disconnect");
        }
      }
    };
    this.xhr.send("data=" + escape(message));
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
  makeUrl: function (method) {
    var paths = { GET: "/xhr/" + (+ new Date),
                  POST: "/xhr" };
    return this.url(paths[method]);
  },
  request: function (method, url) {
    var req = this.makeXhr();
    req.open(method, url);
    if (method == 'POST' && 'setRequestHeader' in req){
      req.setRequestHeader('Content-type', 'application/x-www-form-urlencoded; charset=utf-8');
    }
    return req;
  }
};

