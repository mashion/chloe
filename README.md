Chloe
=====

A realtime web server that doesn't suck.

Caveats
-------

Chloe currently only works with browsers that support websockets. We've tested
it with Safari and Chrome. We'll be rolling out support for more browsers in
the near future.

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
