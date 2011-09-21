Chloe
=====

A realtime web server that doesn't suck... or at least won't suck when it's
finished.

How it Works
------------

The User's browser loads your page [1], which instantiates a connection to
Chloe using JavaScript. It can then send data over that connection [2], which
will be relayed to your app via a POST from Chloe [3]. When you have data that
you want to send back to the browser, send a POST to Chloe [4], which will
relay it back to the connected browser [5].

                               2. Send data over websockets
      +---------------------------------------------------------------------------------------------+
      |                                                                                             v
    +------------------------+  1. /index.html   +----------+  4. POST /send (data for browser)   +-------+
    |        Browser         | ----------------> | Your App | ----------------------------------> | Chloe | -+
    +------------------------+                   +----------+                                     +-------+  |
      ^                                            ^          3. Data from the browser              |        |
      | 5. Data from your app                      +------------------------------------------------+        |
      |                                                                                                      |
      |                                                                                                      |
      +------------------------------------------------------------------------------------------------------+

Support
-------

Mailing List: `http://groups.google.com/group/chloe-ws`

Installation
------------

Mac Binary
==========

    curl -LO https://github.com/downloads/mashion/chloe/chloe-0.0.5-osx.tgz
    tar xzvf chloe-0.0.5-osx.tgz
    cd chloe-0.0.5
    ./bin/chloe start

Ubuntu Binary
=============

    wget https://github.com/downloads/mashion/chloe/chloe-0.0.5-ubuntu-32.tgz
    tar xzvf chloe-0.0.5-ubuntu32.tgz
    cd chloe-0.0.5
    ./bin/chloe start

From Source
===========

- Get you some erlang, on a mac:

        brew install erlang

- Get rebar: [start here](https://github.com/basho/rebar/wiki/Getting-started). Or for mac:

        brew install rebar

- Clone this repo:

        git clone https://github.com/mashion/chloe.git
        cd chloe

- Run these commands

        gem install rake
        rake bootstrap
        rake server

- get sinatra and run the demo app in another terminal window

        gem install sinatra
        rake demo

- Point your browser at http://localhost:4567 and open up the javascript console
- demo.js sets up a chloe variable that is already connected to the server, do
  the following in the console:

        chloe.send("hi mom");

- Relish the awesome.
- To stop Chloe, enter `q().` back in erlang
- To stop the demo app, use Ctrl+C

JS API
------

Instantiate a Chloe object:

    var chloe = new Chloe({host: 'localhost', port: 8901});

Define a function for handling incoming messages:

    chloe.onmessage(function (message) {
      console.log('I got a message: ' + message);
    });

Connect to Chloe and send a message:

    chloe.connect(function () {
      chloe.send('Ohai!');
    });

Subscribe to a channel (note that we do this within the `connect` callback,
because subscribing to a channel requires sending a message to Chloe).

    chloe.connect(function () {
      chloe.subscribe('pumpkin', function (message) {
        console.log('Someone was eating pumpkins: ' + message);
      });
    });

Server Side API
---------------

Sending a message to Chloe, which will then be sent to all connected browsers:

    curl -d "data=This is the message data" http://localhost:8901/send

Sending a message to a specific channel in chloe:

    curl -d "channel=pumpkin&data=Trotter" http://localhost:8901/send

Configuring Chloe
-----------------

Chloe has a single configuration file, `chloe-0.0.5/etc/app.config`. Allowed
options for the Chloe application are:

  - **application_server_url**: The url for the application server, ex: http://localhost:4567/updates
  - **port**: The port on which chloe runs (default: 8901)
  - **log_dir**: Where the Chloe log files will go (default: `.`)

Transport Types
---------------

Chloe currently supports Websockets, XHR, and JSONP as possible transports. If your
browser does not support WebSockets, we'll intelligently fallback to JSONP or XHR.

Caveats
-------

Our primary test browser is Chrome. If you're using any other browser, please
let us know if you run into issues.

This sucker is pretty alpha right now. There's a number of undocumented
features, but everything that is documented seems to work. As with all alpha,
open source software, ymmv. Pull requests gladly accepted!
