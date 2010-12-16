-module(chloe_socketio_protocol_test).

-include_lib("eunit/include/eunit.hrl").

parse_message_with_empty_realm_test() ->
    {ok, Type, Realm, Data} = chloe_socketio_protocol:parse(<<"1:6::hello,">>),
    message = Type,
    ""      = Realm,
    "hello" = Data.

