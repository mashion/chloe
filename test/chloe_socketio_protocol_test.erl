-module(chloe_socketio_protocol_test).

-include_lib("eunit/include/eunit.hrl").

parse_message_test() ->
    {ok, Type, Data} = chloe_socketio_protocol:parse(<<"1:5:hello,">>),
    message = Type,
    "hello" = Data.

