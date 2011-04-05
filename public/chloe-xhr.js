Chloe.XhrTransport = function (options) {
  Chloe.XDomainTransport.apply(this, [options]);
};


Chloe.XhrTransport.isEnabled = function (host) {
  return ('XMLHttpRequest' in window) &&
         ((new XMLHttpRequest()).withCredentials != undefined);
};

Chloe.XhrTransport.prototype = {
  makeXhr: function () {
    return new XMLHttpRequest();
  }
};

Chloe.XDomainTransport.include(Chloe.XhrTransport.prototype);