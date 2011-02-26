Chloe.Message = function (options) {
  this.data     = options.data;
  this.packed = options.packed;
};

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
    this.packed = this.data;
  },
  unpack: function () {
    this.data = this.packed;
  }
};
