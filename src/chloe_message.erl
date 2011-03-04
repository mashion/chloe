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

unpack(Data) ->
    {ok, {struct, PropList}} = json:decode_string(binary_to_list(Data)),
    check_version(PropList),
    #message{data=proplists:get_value(data, PropList),
             version=proplists:get_value(version, PropList),
             type=proplists:get_value(type, PropList),
             channel=proplists:get_value(channel, PropList)}.

pack(Message) ->
    json:encode({struct, [{data,    Message#message.data},
                          {version, ?VERSION},
                          {type,    Message#message.type},
                          {channel, Message#message.channel}]}).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

check_version(PropList) ->
    ?VERSION = proplists:get_value(version, PropList).

