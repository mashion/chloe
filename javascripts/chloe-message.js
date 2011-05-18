// Chloe message types:
// 'connect'
// 'channel-subscribe'
// 'message'
// 'poll'

Chloe.Message = function (options) {
  this.version = Chloe.Message.version;
  this.sessionId = options.sessionId;
  this.id      = options.id;
  this.type    = options.type;
  // TODO (trotter): I don't really like doing this, find a better way.
  this.channel = options.channel;
  this.data    = options.data;
  this.packed  = options.packed;
};

Chloe.Message.version = 1;

Chloe.Message.pack = function (data, sessionId) {
  var message = new Chloe.Message({data: data,
                                   type: "message",
                                   sessionId: sessionId});
  message.pack();
  return message;
};

Chloe.Message.unpack = function (packed) {
  var message = new Chloe.Message({packed: packed});
  message.unpack();
  return message;
};

Chloe.Message.channelSubscribe = function (channel, client) {
  var message = new Chloe.Message({type: "channel-subscribe",
                                   sessionId: client.sessionId,
                                   channel: channel});
  message.pack();
  return message;
};

Chloe.Message.prototype = {
  pack: function () {
    this.packed = JSON.stringify({
      type:      this.type,
      channel:   this.channel,
      data:      this.data,
      version:   this.version,
      id:        this.id,
      sessionId: this.sessionId
    });
  },
  unpack: function () {
    var decoded = JSON.parse(this.packed);
    if (decoded.version !== this.version) {
      throw new Error("Expected message version " + decoded.version + " to match " + this.version);
    }
    this.data      = decoded.data;
    this.channel   = decoded.channel;
    this.type      = decoded.type;
    this.id        = decoded.id;
    this.sessionId = decoded.sessionId;
  },
  send: function (transport) {
    this.pack();
    transport.send(this.packed);
  }
};
