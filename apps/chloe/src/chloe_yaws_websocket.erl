-module(chloe_yaws_websocket).

-include("../../../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

-define(ACTIVE, true).

out(A) ->
    {ok, WebSocketOwner} = chloe_websocket_sup:start_child(),
    yaws_websockets:handshake(A, WebSocketOwner, true).
