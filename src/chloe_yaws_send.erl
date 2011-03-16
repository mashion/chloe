-module(chloe_yaws_send).

-include("../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

out(A) ->
    send_to_all_subscribers(A),
    {content, "text/plain", "success"}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

send_to_all_subscribers(A) ->
    Channel = case yaws_api:postvar(A, "channel") of
                  {ok, C} -> C;
                  _       -> "/all"
              end,
    {ok, Subscribers} = chloe_channel_store:fetch_subscribers(Channel),
    {ok, Data}        = yaws_api:postvar(A, "data"),
    lists:foreach(
        fun(Pid) ->
            error_logger:info_msg("Sending ~p to ~p~n", [Data, Channel]),
            chloe_session:send_to_browser(Pid, Channel, Data)
        end,
        Subscribers).
