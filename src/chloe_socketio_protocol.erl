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
    {ok, Type, Body} = parse_type(binary_to_list(Data)),
    {ok, Length, Body2} = parse_length(Body),
    {ok, Realm, Length2, Body3} = parse_realm(Length, Body2),
    {ok, Payload} = parse_payload(Length2, Body3),
    {ok, #socketio_msg{type = Type, data = Payload,
                       modifiers = [{realm, Realm}]}}.

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
parse_type([TypeId | [$: | Data]]) ->
    Type = case TypeId of
               $1 -> message
           end,
    {ok, Type, Data}.

parse_length(Data) ->
    {ok, Length, Rest} = split_at_first($:, Data),
    {ok, list_to_integer(Length), Rest}.

parse_realm(Length, Data) ->
    {ok, Realm, Rest} = split_at_first($:, Data),
    % The '- 1' here is because of the ':' we removed
    {ok, Realm, Length - length(Realm) - 1, Rest}.

% The Length + 1 is the length of the message, plus 1 for the ',' at 
% the end
parse_payload(Length, Data) when length(Data) =:= Length + 1 ->
    {ok, lists:sublist(Data, Length)}.

split_at_first(Chr, Data) ->
    split_at_first(Chr, Data, "").

split_at_first(Chr, [Chr | Rest], RetChrs) ->
    {ok, lists:reverse(RetChrs), Rest};
split_at_first(Chr, [RetChr | Data], RetChrs) ->
    split_at_first(Chr, Data, [RetChr | RetChrs]).
