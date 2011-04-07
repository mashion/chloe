Chloe.XDomainTransport = function (options) {
  Chloe.XhrTransport.apply(this, [options]);
};

Chloe.XDomainTransport.isEnabled = function () {
  return 'XDomainRequest' in window;
};

Chloe.XDomainTransport.prototype = Chloe.extend(Chloe.XhrTransport.prototype, {
  makeXhr: function () {
    return new XDomainRequest();
  }
});
