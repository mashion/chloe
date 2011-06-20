-module(chloe_yaws_send).

-include("../../../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

out(A) ->
    case is_message_authenticated(A) of
        true -> send_to_all_subscribers(A),
                {content, "text/plain", "success"};
           _ -> {content, "text/plain", "unauthenticated"}
    end.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

is_message_authenticated(A) ->
    case application:get_env(chloe, secret) of
        undefined    -> true;
        {ok, Secret} -> check_signature(A, Secret)
    end.

check_signature(A, Secret) ->
    {ok, Data} = yaws_api:postvar(A, "data"),
    {ok, Sig}  = yaws_api:postvar(A, "sig"),
    lib_md5:hexdigest(Data ++ Secret) =:= Sig.

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
