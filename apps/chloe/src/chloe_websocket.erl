%% To test run the following in chrome:
%% ws = new WebSocket("ws://localhost:8888/updates");
%% ws.onmessage = function (m) { console.log(m.data); };
%% ws.send("Hey dude");

-module(chloe_websocket).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         send/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {websocket, session_id}).
-include_lib("./chloe.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

send(Pid, [Messages]) ->
    gen_server:cast(Pid, {send, [Messages]}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, [Messages]}, State) ->
    lists:foreach(fun (M) ->
            Packed = chloe_message:pack(M),
            error_logger:info_msg("Sending back: ~p", [Packed]),
            yaws_api:websocket_send(State#state.websocket, Packed)
        end, Messages),
    {noreply, State}.

%% This is where our websocket comms will come in
handle_info({ok, WebSocket}, State) ->
    error_logger:info_msg("Websocket started on ~p~n", [self()]),
    SessionId = perform_session_handshake(WebSocket),
    {noreply, State#state{websocket = WebSocket,
                          session_id = SessionId}};
handle_info({tcp, _WebSocket, DataFrames}, State) ->
    error_logger:info_msg("Raw DataFrame: ~p~n", [DataFrames]),
    handle_websocket_frames(DataFrames, State),
    {noreply, State};
handle_info({tcp_closed, _WebSocket}, State) ->
    {stop, ok, State};
% handle_info(discard, State) ->
%     {stop, ok, State};
%% httpc is going to tell us how our request went,
%% but we don't care
handle_info({http, _Response}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

perform_session_handshake(_WebSocket) ->
    {ok, SessionId} = chloe_session_manager:create(self()),
%    Message = chloe_socketio_protocol:pack(handshake, SessionId),
%    yaws_api:websocket_send(WebSocket, Message),
    SessionId.

session_pid(SessionId) ->
    {ok, SessionPid} = chloe_session_manager:fetch_pid(SessionId),
    SessionPid.

handle_message(Data, State) ->
    Message = chloe_message:unpack(Data),
    case Message#message.type of
        "channel-subscribe" -> handle_channel_subscribe_message(Message, State);
        _ -> handle_data_message(Message, State)
    end.

handle_websocket_frames(DataFrames, State) ->
    %% TODO (trotter): We _may_ be able to use yaws_websockets:unframe_all
    %%                 then process the resulting list.
    case yaws_websockets:unframe_one(DataFrames) of
        {ok, Data, <<>>}      -> handle_message(Data, State);
        {ok, Data, NextFrame} -> handle_message(Data, State),
                                 handle_websocket_frames(NextFrame, State)
    end.

handle_channel_subscribe_message(Message, State) ->
    chloe_session:subscribe(session_pid(State#state.session_id),
                            Message#message.channel).

handle_data_message(Message, State) ->
    error_logger:info_msg("Got data from WebSocket: ~p~n", [Message#message.data]),
    chloe_session:send_to_server(session_pid(State#state.session_id),
                                 Message#message.data).
