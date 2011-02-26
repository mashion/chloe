Chloe.Message = function (options) {
  this.version = Chloe.Message.version;
  this.data    = options.data;
  this.packed  = options.packed;
};

Chloe.Message.version = 1;

Chloe.Message.pack = function (data) {
  var message = new Chloe.Message({data: data});
  message.pack();
  return message;
};

Chloe.Message.unpack = function (packed) {
  var message = new Chloe.Message({packed: packed});
  message.unpack();
  return message;
}

Chloe.Message.prototype = {
  pack: function () {
    this.packed = JSON.stringify({
      data: this.data,
      version: this.version
    });
  },
  unpack: function () {
    var decoded = JSON.parse(this.packed);
    if (decoded.version != this.version) {
      throw new Error("Expected message version " + decoded.version + " to match " + this.version);
    }
    this.data = decoded.data;
  }
};
