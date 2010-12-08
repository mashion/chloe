-module(chloe_yaws_send).

-include("../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

out(A) ->
    send_to_all_subscribers(A#arg.clidata),
    {content, "text/plain", "success"}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

send_to_all_subscribers(Data) ->
    {ok, Subscribers} = chloe_channel_store:fetch_subscribers("/all"),
    lists:foreach(
        fun(Pid) ->
            chloe_websocket:send(Pid, Data)
        end,
        Subscribers).
