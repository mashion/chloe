Chloe.Message = function (options) {
  this.version = Chloe.Message.version;
  this.type    = options.type;
  // TODO (trotter): I don't really like doing this, find a better way.
  this.channel = options.channel;
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

Chloe.Message.channelSubscribe = function (channel) {
  var message = new Chloe.Message({type: "channel-subscribe",
                                   channel: channel});
  message.pack();
  return message;
}

Chloe.Message.prototype = {
  pack: function () {
    this.packed = JSON.stringify({
      type:    this.type,
      channel: this.channel,
      data:    this.data,
      version: this.version
    });
  },
  unpack: function () {
    var decoded = JSON.parse(this.packed);
    if (decoded.version != this.version) {
      throw new Error("Expected message version " + decoded.version + " to match " + this.version);
    }
    this.data    = decoded.data;
    this.channel = decoded.channel;
    this.type    = decoded.type;
  },
  send: function (transport) {
    transport.send(this.packed);
  }
};
