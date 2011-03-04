-module(chloe_message).

%% API
-export([
         unpack/1,
         pack/1
        ]).

-define(VERSION, 1).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

unpack(Data) ->
    {ok, {struct, PropList}} = json:decode_string(binary_to_list(Data)),
    check_version(PropList),
    proplists:get_value(data, PropList).

pack(Data) ->
    json:encode({struct, [{data, Data}, {version, ?VERSION}]}).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

check_version(PropList) ->
    ?VERSION = proplists:get_value(version, PropList).

