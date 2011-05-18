/*
    http://www.JSON.org/json2.js
    2011-02-23

    Public Domain.

    NO WARRANTY EXPRESSED OR IMPLIED. USE AT YOUR OWN RISK.

    See http://www.JSON.org/js.html


    This code should be minified before deployment.
    See http://javascript.crockford.com/jsmin.html

    USE YOUR OWN COPY. IT IS EXTREMELY UNWISE TO LOAD CODE FROM SERVERS YOU DO
    NOT CONTROL.


    This file creates a global JSON object containing two methods: stringify
    and parse.

        JSON.stringify(value, replacer, space)
            value       any JavaScript value, usually an object or array.

            replacer    an optional parameter that determines how object
                        values are stringified for objects. It can be a
                        function or an array of strings.

            space       an optional parameter that specifies the indentation
                        of nested structures. If it is omitted, the text will
                        be packed without extra whitespace. If it is a number,
                        it will specify the number of spaces to indent at each
                        level. If it is a string (such as '\t' or '&nbsp;'),
                        it contains the characters used to indent at each level.

            This method produces a JSON text from a JavaScript value.

            When an object value is found, if the object contains a toJSON
            method, its toJSON method will be called and the result will be
            stringified. A toJSON method does not serialize: it returns the
            value represented by the name/value pair that should be serialized,
            or undefined if nothing should be serialized. The toJSON method
            will be passed the key associated with the value, and this will be
            bound to the value

            For example, this would serialize Dates as ISO strings.

                Date.prototype.toJSON = function (key) {
                    function f(n) {
                        return n < 10 ? '0' + n : n;
                    }

                    return this.getUTCFullYear()   + '-' +
                         f(this.getUTCMonth() + 1) + '-' +
                         f(this.getUTCDate())      + 'T' +
                         f(this.getUTCHours())     + ':' +
                         f(this.getUTCMinutes())   + ':' +
                         f(this.getUTCSeconds())   + 'Z';
                };

            You can provide an optional replacer method. It will be passed the
            key and value of each member, with this bound to the containing
            object. The value that is returned from your method will be
            serialized. If your method returns undefined, then the member will
            be excluded from the serialization.

            If the replacer parameter is an array of strings, then it will be
            used to select the members to be serialized. It filters the results
            such that only members with keys listed in the replacer array are
            stringified.

            Values that do not have JSON representations, such as undefined or
            functions, will not be serialized. Such values in objects will be
            dropped; in arrays they will be replaced with null. You can use
            a replacer function to replace those with JSON values.
            JSON.stringify(undefined) returns undefined.

            The optional space parameter produces a stringification of the
            value that is filled with line breaks and indentation to make it
            easier to read.

            If the space parameter is a non-empty string, then that string will
            be used for indentation. If the space parameter is a number, then
            the indentation will be that many spaces.

            Example:

            text = JSON.stringify(['e', {pluribus: 'unum'}]);


            text = JSON.stringify(['e', {pluribus: 'unum'}], null, '\t');

            text = JSON.stringify([new Date()], function (key, value) {
                return this[key] instanceof Date ?
                    'Date(' + this[key] + ')' : value;
            });


        JSON.parse(text, reviver)
            This method parses a JSON text to produce an object or array.
            It can throw a SyntaxError exception.

            The optional reviver parameter is a function that can filter and
            transform the results. It receives each of the keys and values,
            and its return value is used instead of the original value.
            If it returns what it received, then the structure is not modified.
            If it returns undefined then the member is deleted.

            Example:


            myData = JSON.parse(text, function (key, value) {
                var a;
                if (typeof value === 'string') {
                    a =
/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d*)?)Z$/.exec(value);
                    if (a) {
                        return new Date(Date.UTC(+a[1], +a[2] - 1, +a[3], +a[4],
                            +a[5], +a[6]));
                    }
                }
                return value;
            });

            myData = JSON.parse('["Date(09/09/2001)"]', function (key, value) {
                var d;
                if (typeof value === 'string' &&
                        value.slice(0, 5) === 'Date(' &&
                        value.slice(-1) === ')') {
                    d = new Date(value.slice(5, -1));
                    if (d) {
                        return d;
                    }
                }
                return value;
            });


    This is a reference implementation. You are free to copy, modify, or
    redistribute.
*/

/*jslint evil: true, strict: false, regexp: false */

/*members "", "\b", "\t", "\n", "\f", "\r", "\"", JSON, "\\", apply,
    call, charCodeAt, getUTCDate, getUTCFullYear, getUTCHours,
    getUTCMinutes, getUTCMonth, getUTCSeconds, hasOwnProperty, join,
    lastIndex, length, parse, prototype, push, replace, slice, stringify,
    test, toJSON, toString, valueOf
*/



var JSON;
if (!JSON) {
    JSON = {};
}

(function () {
    "use strict";

    function f(n) {
        return n < 10 ? '0' + n : n;
    }

    if (typeof Date.prototype.toJSON !== 'function') {

        Date.prototype.toJSON = function (key) {

            return isFinite(this.valueOf()) ?
                this.getUTCFullYear()     + '-' +
                f(this.getUTCMonth() + 1) + '-' +
                f(this.getUTCDate())      + 'T' +
                f(this.getUTCHours())     + ':' +
                f(this.getUTCMinutes())   + ':' +
                f(this.getUTCSeconds())   + 'Z' : null;
        };

        String.prototype.toJSON      =
            Number.prototype.toJSON  =
            Boolean.prototype.toJSON = function (key) {
                return this.valueOf();
            };
    }

    var cx = /[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
        escapable = /[\\\"\x00-\x1f\x7f-\x9f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
        gap,
        indent,
        meta = {    // table of character substitutions
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        },
        rep;


    function quote(string) {


        escapable.lastIndex = 0;
        return escapable.test(string) ? '"' + string.replace(escapable, function (a) {
            var c = meta[a];
            return typeof c === 'string' ? c :
                '\\u' + ('0000' + a.charCodeAt(0).toString(16)).slice(-4);
        }) + '"' : '"' + string + '"';
    }


    function str(key, holder) {


        var i,          // The loop counter.
            k,          // The member key.
            v,          // The member value.
            length,
            mind = gap,
            partial,
            value = holder[key];


        if (value && typeof value === 'object' &&
                typeof value.toJSON === 'function') {
            value = value.toJSON(key);
        }


        if (typeof rep === 'function') {
            value = rep.call(holder, key, value);
        }


        switch (typeof value) {
        case 'string':
            return quote(value);

        case 'number':


            return isFinite(value) ? String(value) : 'null';

        case 'boolean':
        case 'null':


            return String(value);


        case 'object':


            if (!value) {
                return 'null';
            }


            gap += indent;
            partial = [];


            if (Object.prototype.toString.apply(value) === '[object Array]') {


                length = value.length;
                for (i = 0; i < length; i += 1) {
                    partial[i] = str(i, value) || 'null';
                }


                v = partial.length === 0 ? '[]' : gap ?
                    '[\n' + gap + partial.join(',\n' + gap) + '\n' + mind + ']' :
                    '[' + partial.join(',') + ']';
                gap = mind;
                return v;
            }


            if (rep && typeof rep === 'object') {
                length = rep.length;
                for (i = 0; i < length; i += 1) {
                    if (typeof rep[i] === 'string') {
                        k = rep[i];
                        v = str(k, value);
                        if (v) {
                            partial.push(quote(k) + (gap ? ': ' : ':') + v);
                        }
                    }
                }
            } else {


                for (k in value) {
                    if (Object.prototype.hasOwnProperty.call(value, k)) {
                        v = str(k, value);
                        if (v) {
                            partial.push(quote(k) + (gap ? ': ' : ':') + v);
                        }
                    }
                }
            }


            v = partial.length === 0 ? '{}' : gap ?
                '{\n' + gap + partial.join(',\n' + gap) + '\n' + mind + '}' :
                '{' + partial.join(',') + '}';
            gap = mind;
            return v;
        }
    }


    if (typeof JSON.stringify !== 'function') {
        JSON.stringify = function (value, replacer, space) {


            var i;
            gap = '';
            indent = '';


            if (typeof space === 'number') {
                for (i = 0; i < space; i += 1) {
                    indent += ' ';
                }


            } else if (typeof space === 'string') {
                indent = space;
            }


            rep = replacer;
            if (replacer && typeof replacer !== 'function' &&
                    (typeof replacer !== 'object' ||
                    typeof replacer.length !== 'number')) {
                throw new Error('JSON.stringify');
            }


            return str('', {'': value});
        };
    }



    if (typeof JSON.parse !== 'function') {
        JSON.parse = function (text, reviver) {


            var j;

            function walk(holder, key) {


                var k, v, value = holder[key];
                if (value && typeof value === 'object') {
                    for (k in value) {
                        if (Object.prototype.hasOwnProperty.call(value, k)) {
                            v = walk(value, k);
                            if (v !== undefined) {
                                value[k] = v;
                            } else {
                                delete value[k];
                            }
                        }
                    }
                }
                return reviver.call(holder, key, value);
            }



            text = String(text);
            cx.lastIndex = 0;
            if (cx.test(text)) {
                text = text.replace(cx, function (a) {
                    return '\\u' +
                        ('0000' + a.charCodeAt(0).toString(16)).slice(-4);
                });
            }



            if (/^[\],:{}\s]*$/
                    .test(text.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g, '@')
                        .replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g, ']')
                        .replace(/(?:^|:|,)(?:\s*\[)+/g, ''))) {


                j = eval('(' + text + ')');


                return typeof reviver === 'function' ?
                    walk({'': j}, '') : j;
            }


            throw new SyntaxError('JSON.parse');
        };
    }
}());
Chloe = function (options) {
  options = options || {};
  options.host = options.host || 'localhost';
  options.port = options.port || 8901;

  var transports = [Chloe.WebSocketTransport,
                    Chloe.XhrTransport,
                    Chloe.JsonpTransport ];

  for (var i = 0, l = transports.length; i < l; i++) {
    if (transports[i].isEnabled()) {
      this.transport = new transports[i](options);
      break;
    }
  }

  this.channelSubscriptions = {};
};

Chloe.extend = function (source, obj) {
  for (var prop in source) obj[prop] = source[prop];
  return obj;
};

Chloe.prototype = {
  connect: function (callback) {
    var self = this;
    this.transport.connect(function (data) {
      self.sessionId = data.sessionId;
      callback();
    });
  },
  onmessage: function (callback) {
    var self = this;
    this.onmessageCallback = callback;
    this.transport.onmessage(function (message) {
      self.handleMessage(Chloe.Message.unpack(message));
    });
  },
  onclose: function (callback) {
    this.transport.onclose(callback);
  },
  send: function (data) {
    var message = Chloe.Message.pack(data, this.sessionId);
    message.send(this.transport);
  },
  subscribe: function (channel, callback) {
    var message = Chloe.Message.channelSubscribe(channel, this);
    this.channelSubscriptions[channel] = callback;
    message.send(this.transport);
  },

  handleMessage: function (message) {
    var callback = this.channelSubscriptions[message.channel];
    if (callback) {
      callback(message.data);
    } else {
      this.onmessageCallback(message.data);
    }
  }
};
Chloe.WebSocketTransport = function (options) {
  this.host = options.host;
  this.port = options.port;
  this.socketAttributes = {};
};

Chloe.WebSocketTransport.prototype = {
  connect: function (callback) {
    var self = this;
    this.socket = new WebSocket("ws://" + this.host + ":" + this.port + "/chloe/websocket");
    this.socket.onopen = callback;
    for (var i in this.socketAttributes) {
      this.socket[i] = this.socketAttributes[i];
    }
  },
  onclose: function (callback) {
    this.attachToSocket('onclose', callback);
  },
  onmessage: function (callback) {
    this.attachToSocket('onmessage', function (message) {
      callback(message.data);
    });
  },
  send: function (message) {
    this.socket.send(message);
  },

  attachToSocket: function (attribute, callback) {
    this.socketAttributes[attribute] = callback;
    if (this.socket) {
      this.socket[attribute] = this.socketAttributes[attribute];
    }
  }
};

Chloe.WebSocketTransport.isEnabled = function () {
  return typeof(WebSocket) === "function";
};
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
      self.listenForMessages();
    });
  }
};
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
Chloe.JsonpTransport = function (options) {
  this.host = options.host;
  this.port = options.port;
  this.protocol = "http://";
  this.callbacks = {};
  this.register();
};

Chloe.JsonpTransport.prototype = {
  connect: function (callback) {
    var self = this,
        message = new Chloe.Message({
          id: this.id,
          type: 'connect'
        });

    this.callbacks.onconnect = function (data) {
      self.sessionId = data.sessionId;
      self.listenForMessages();
      callback(data);
    };

    message.pack();
    message.send(this);
  },

  onmessage: function (callback) {
    this.callbacks.onmessage = callback;
  },

  onclose: function (callback) {
    this.callbacks.onclose = callback;
  },

  send: function (data, options) {
    var self = this,
        script = document.createElement('script');

    options = options || {};

    script.src = this.url(data);
    script.type = 'text/javascript';
    script.onerror = function () {
      if (options.onerror) {
        options.onerror();
      }
      self.handleError();
    };
    document.body.appendChild(script);
  },

  register: function () {
    this.id   = (new Date()).getTime();
    Chloe.JsonpTransport.connections[this.id] = this;
  },

  url: function (data) {
    return this.protocol + this.host + ":" + this.port + "/chloe/jsonp.js?data=" + escape(data) + "ts=" + (new Date()).getTime();
  },

  handleError: function () {
  },

  listenForMessages: function () {
    var self = this,
        message = new Chloe.Message({
                                      id: this.id,
                                      sessionId: this.sessionId,
                                      type: "poll"
                                    });
    message.send(this, {onerror: function () { self.listenForMessages(); }});
  }
};

Chloe.JsonpTransport.connections = {};

Chloe.JsonpTransport.isEnabled = function () {
  return true;
}

Chloe.JsonpTransport.response = function (data) {
  var message    = new Chloe.Message(data),
      connection = Chloe.JsonpTransport.connections[message.id];

  message.pack();
  if (message.type === 'connect') {
    connection.callbacks.onconnect(message);
  } else if (message.type === 'poll') {
    connection.callbacks.onmessage(message.packed);
    connection.listenForMessages();
  } else {
    throw new Error("Unknown message type for JsonpTransport.");
  }
};

Chloe.Message = function (options) {
  this.version = Chloe.Message.version;
  this.sessionId = options.sessionId;
  this.id      = options.id;
  this.type    = options.type;
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
