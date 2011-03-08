Chloe.Transport.XHR = function (options) {
  Chloe.Transport.Base.mixin(this);
  this.host = options.host;
  this.port = options.port;
  this.protocol = "http";
};

Chloe.Transport.XHR.prototype = {
  // Transport functions
  connect: function (callback) {
    var self = this;
    this.detectXhrTechnique();
    this.xhr = this.request('GET');
    this.xhr.onreadystatechange = function(){
      var status;
      if (self.xhr.readyState == 4){
        self.xhr.onreadystatechange = function() {};
        try { status = self.xhr.status; } catch(e){}
        if (status == 200){
          console.log(self.xhr.responseText);
        } else {
          debugger;
          console.log("disconnect");
        }
      }
    };
    this.xhr.send(null);
  },
  onclose: function (callback) {
  },
  onmessage: function (callback) {
  },
  send: function (message) {
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
                  POST: "/send" };
    return this.url(paths[method]);
  },
  request: function (method) {
    var req = this.makeXhr();
    req.open(method, this.makeUrl(method));
    if (method == 'POST' && 'setRequestHeader' in req){
      req.setRequestHeader('Content-type', 'application/x-www-form-urlencoded; charset=utf-8');
    }
    return req;
  }
};

