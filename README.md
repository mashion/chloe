Chloe
=====

A realtime web server that doesn't suck.

Installation
------------

- Get you some erlang
- Get rebar (it's on github)
- clone this repo
- `cd chloe && rebar compile`
- `erl -pa ebin -pa deps/yaws/ebin`
- in erl run `chloe:start()`
- in another terminal window, `ruby support/echo_server.rb`
- Point your browser at http://localhost:8888 and open up the javascript console
- In the console:
    ws = new WebSocket("ws://localhost:8888/updates");
    ws.onmessage = function (m) { console.log(m.data) };
    ws.send("Patch me through");
- Relish in the awesome.
