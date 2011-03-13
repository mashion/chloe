-module(chloe_yaws_jsonp).

-include("../deps/yaws/include/yaws_api.hrl").
-include_lib("./chloe.hrl").

-export([out/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

out(A) ->
    Raw = proplists:get_value("data", yaws_api:parse_query(A)),
    Message = chloe_message:unpack(Raw),
    case Message#message.type of
        "connect" -> handle_connect(Message)
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_connect(Message) ->
    error_logger:info_msg("The message: ~n~p", [Message]),
    SessionId = create_session(),
    error_logger:info_msg("Made our session"),
    Packed = chloe_message:pack(#message{session_id=SessionId,
                                         id=Message#message.id,
                                         type=Message#message.type}),
    error_logger:info_msg("Packed our message"),
    {content,
     "application/javascript",
     string:join(["Chloe.JsonpTransport.response(", Packed, ");"], "")}.

create_session() ->
    %% TODO: We should really tell chloe session manager what it's dealing
    %%       with, so that it doesn't try to send data down the pipe.
    {ok, SessionId} = chloe_session_manager:create(self()),
    SessionId.
