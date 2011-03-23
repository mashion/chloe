-module(chloe_message).

%% API
-export([
         unpack/1,
         pack/1
        ]).

-define(VERSION, 1).
-include_lib("./chloe.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

unpack(Data) when is_binary(Data) ->
    unpack(binary_to_list(Data));
unpack(Data) ->
    {ok, {struct, PropList}} = json:decode_string(Data),
    check_version(PropList),
    #message{data=proplists:get_value(data, PropList),
             version=proplists:get_value(version, PropList),
             type=proplists:get_value(type, PropList),
             channel=proplists:get_value(channel, PropList),
             id=proplists:get_value(id, PropList),
             session_id=proplists:get_value(sessionId, PropList)}.

to_struct(Message) ->
    {struct, [{data,      Message#message.data},
              {version,   ?VERSION},
              {type,      Message#message.type},
              {channel,   Message#message.channel},
              {id,        Message#message.id},
              {sessionId, Message#message.session_id}]}.

pack(Messages) when is_list(Messages) ->
  json:encode({struct, [{messages, {array, lists:map(fun (M) ->
                           to_struct(M)
                       end, Messages)}}]});
pack(Message) ->
  json:encode(to_struct(Message)).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

check_version(PropList) ->
    ?VERSION = proplists:get_value(version, PropList).

