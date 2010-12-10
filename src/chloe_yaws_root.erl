-module(chloe_yaws_root).

-include("../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

out(A) when A#arg.pathinfo =:= "/js/socket.io.js" ->
    {ok, Data} = file:read_file("support/js/socket.io.js"),
    {content, "application/javascript", Data};
out(A) when A#arg.pathinfo =:= "/js/setup.js" ->
    {ok, Data} = file:read_file("support/js/setup.js"),
    {content, "application/javascript", Data};
out(_A) ->
    {ehtml, [{head, [], [{script, [{src, "/js/socket.io.js"}, {type, "application/javascript"}], ""},
                         {script, [{src, "/js/setup.js"}, {type, "application/javascript"}], ""}]},
             {h1, [], "Chloe"}]}.
