-module(chloe_yaws_jsonp).

-include("../deps/yaws/include/yaws_api.hrl").
-include_lib("./chloe.hrl").

-export([out/1]).

-define(MIME_TYPE, "application/javascript").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

out(A) ->
    Raw = proplists:get_value("data", yaws_api:parse_query(A)),
    Message = chloe_message:unpack(Raw),
    case Message#message.type of
        "connect"           -> handle_connect(Message);
        "message"           -> handle_message(Message);
        "channel-subscribe" -> handle_channel_subscribe(Message);
        "poll"              -> handle_poll(A, Message)
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_connect(Message) ->
    SessionId = create_session(),
    Packed = chloe_message:pack(#message{session_id=SessionId,
                                         id=Message#message.id,
                                         type=Message#message.type}),
    {content, ?MIME_TYPE, jsonp_response(Packed)}.

handle_message(Message) ->
    chloe_session:send_to_server(session_pid(Message#message.session_id),
                                 Message#message.data),
    {content, ?MIME_TYPE, ""}.

handle_channel_subscribe(Message) ->
    chloe_session:subscribe(session_pid(Message#message.session_id),
                            Message#message.channel),
    {content, ?MIME_TYPE, ""}.

handle_poll(A, Message) ->
    {ok, JsonpStreamPid} = chloe_jsonp_stream_sup:start_child(A#arg.clisock, Message),
    {streamcontent_from_pid, ?MIME_TYPE, JsonpStreamPid}.

%%     Messages = chloe_session:retrieve_messages(session_pid(Message#message.session_id)),
%%     error_logger:info_msg("We got messages ~p~n", [Messages]),
%%     PackedMessages = lists:map(fun (M) ->
%%             Packed = chloe_message:pack(#message{id=Message#message.id,
%%                                                  type=Message#message.type,
%%                                                  data=M#message.data,
%%                                                  channel=M#message.channel}),
%%             jsonp_response(Packed)
%%         end, Messages),
%%     Packed = string:join(PackedMessages, ""),
%%     {content, "application/javascript", Packed}.

jsonp_response(Packed) ->
    string:join(["Chloe.JsonpTransport.response(", Packed, ");"], "").

session_pid(SessionId) ->
    {ok, SessionPid} = chloe_session_manager:fetch_pid(SessionId),
    SessionPid.

create_session() ->
    %% TODO: We should really tell chloe session manager what it's dealing
    %%       with, so that it doesn't try to send data down the pipe.
    {ok, SessionId} = chloe_session_manager:create(undefined),
    SessionId.
