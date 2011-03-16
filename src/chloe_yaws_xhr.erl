-module(chloe_yaws_xhr).

-include("../deps/yaws/include/yaws_api.hrl").
-include_lib("./chloe.hrl").

-export([out/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

out(A) ->
    Raw = proplists:get_value("data", yaws_api:parse_post(A)),
    Message = chloe_message:unpack(Raw),
    case Message#message.type of
        "connect"           -> handle_connect(Message);
        "message"           -> handle_message(Message)
        %%"channel-subscribe" -> handle_channel_subscribe(Message);
        %%"poll"              -> handle_poll(Message)
    end.

handle_connect(Message) ->
    SessionId = create_session(),
    Packed = chloe_message:pack(#message{session_id=SessionId,
                                         id=Message#message.id,
                                         type=Message#message.type}),
    {ok, Origin} = application:get_env(chloe, application_server),
    [{header, ["Access-Control-Allow-Origin: ", Origin]},
     {content, "application/json", ""}].

handle_message(Message) ->
    {content, "application/javascript", ""}.

create_session() ->
    %% TODO: We should really tell chloe session manager what it's dealing
    %%       with, so that it doesn't try to send data down the pipe.
    {ok, SessionId} = chloe_session_manager:create(self()),
    SessionId.
