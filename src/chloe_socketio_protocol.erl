-module(chloe_socketio_protocol).

%% API
-export([
         parse/1,
         pack/2,
         pack/3
        ]).

-include_lib("./chloe.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

parse(Data) ->
    parse(Data, #socketio_msg{}).

pack(message, Realm, Data) when is_binary(Data) ->
    pack(message, Realm, binary_to_list(Data));
pack(message, Realm, Data) ->
    Body = lists:append([Realm, ":", Data]),
    lists:append(["1:", integer_to_list(length(Body)), ":", Body, ","]).

pack(handshake, SessionId) ->
    lists:append(["3:", integer_to_list(length(SessionId)), ":", SessionId, ","]).

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
%% NOTE: This parse works as long as the realm annotation of the
%%       Socket.IO protocol is not used. If that annotation is used,
%%       it will inject an extra ':', which gums up the whole works.
parse(Data, #socketio_msg{type=undefined} = Message) ->
    [TypePart, Rest] = binary:split(Data, <<":">>),
    Type = case list_to_integer(binary_to_list(TypePart)) of
               1 -> message
           end,
    parse(Rest, Message#socketio_msg{type=Type});
parse(Data, #socketio_msg{data=undefined} = Message) ->
    [Length, Rest] = binary:split(Data, <<":">>),
    [Annotations, Body] = binary:split(Rest, <<":">>),
    BodyLength = list_to_integer(binary_to_list(Length)) - size(Annotations),
    BodyLength = size(Body),
    {ok, Message#socketio_msg{data=binary:part(Body, {0, BodyLength - 1})}}.

