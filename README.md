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

Installation
------------

- Get you some erlang (brew install)
- Get rebar (brew install or [start here](https://github.com/basho/rebar/wiki/Getting-started))
- clone this repo
- Run these commands

        rake bootstrap
        rake server

- get sinatra (for the ruby demo app, gem install sinatra)
- in another terminal window, `rake demo`
- Point your browser at http://localhost:4567 and open up the javascript console
- demo.js sets up a chloe variable that is already connected to the server, do
  the following in the console:

        chloe.send("hi mom");

- Relish the awesome.
- To stop Chloe, enter "q()." back in erlang
- To stop the demo app, use Ctrl+C

Caveats
-------

Chloe currently only works with browsers that support websockets. We've tested
it with Safari and Chrome. We'll be rolling out support for more browsers in
the near future.

This sucker is pretty alpha right now. There's a number of undocumented
features, but everything that is documented, seems to work. As with all alpha,
open source software, ymmv. Pull requests gladly accepted!
