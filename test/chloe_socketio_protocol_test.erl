-module(chloe_socketio_protocol_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/chloe.hrl").

parse_message_with_empty_realm_test() ->
    {ok, Msg} = chloe_socketio_protocol:parse(<<"1:6::hello,">>),
    message = Msg#socketio_msg.type,
    "hello" = Msg#socketio_msg.data,
    [{realm, []}]      = Msg#socketio_msg.modifiers.

pack_message_test() ->
    "1:6::hello," = chloe_socketio_protocol:pack(message, "", "hello").

pack_handshake_test() ->
    "3:3:256," = chloe_socketio_protocol:pack(handshake, "256").
