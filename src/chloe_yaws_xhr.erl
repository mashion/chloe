-module(chloe_yaws_xhr).

-include("../deps/yaws/include/yaws_api.hrl").
-include_lib("./chloe.hrl").

-export([out/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

out(A) ->
    {ok, Raw} = yaws_api:getvar(A, "data"),
    Message = chloe_message:unpack(Raw),
    case Message#message.type of
        "connect"           -> handle_connect(Message);
        "message"           -> handle_message(Message);
        "channel-subscribe" -> handle_channel_subscribe(Message);
        "poll"              -> handle_poll(Message)
    end.

handle_connect(Message) ->
    SessionId = create_session(),
    Packed = chloe_message:pack(#message{session_id=SessionId,
                                         id=Message#message.id,
                                         type=Message#message.type}),
    cross_origin_response(Packed).

handle_message(Message) ->
    chloe_session:send_to_server(session_pid(Message#message.session_id),
                                 Message#message.data),
    cross_origin_response().

handle_channel_subscribe(Message) ->
    chloe_session:subscribe(session_pid(Message#message.session_id),
                            Message#message.channel),
    cross_origin_response().

handle_poll(Message) ->
    Messages = chloe_session:retrieve_messages(session_pid(Message#message.session_id)),
    error_logger:info_msg("We got messages ~p~n", [Messages]),
    %% TODO (mat): this won't jive for XHR. Need respond with array of messages.
    PackedMessages = lists:map(fun (M) ->
            Packed = chloe_message:pack(#message{id=Message#message.id,
                                                 type=Message#message.type,
                                                 data=M#message.data,
                                                 channel=M#message.channel}),
            Packed
        end, Messages),
    Packed = string:join(PackedMessages, ""),
    cross_origin_response(Packed).

create_session() ->
    %% TODO: We should really tell chloe session manager what it's dealing
    %%       with, so that it doesn't try to send data down the pipe.
    {ok, SessionId} = chloe_session_manager:create(undefined),
    SessionId.

session_pid(SessionId) ->
    {ok, SessionPid} = chloe_session_manager:fetch_pid(SessionId),
    SessionPid.

cross_origin_response() ->
    cross_origin_response("").

cross_origin_response(Packed) ->
    {ok, Origin} = application:get_env(chloe, application_server),
    [{header, ["Access-Control-Allow-Origin: ", Origin]},
     {content, "application/json", Packed}].
