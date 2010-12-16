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

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Data}, State) ->
    error_logger:info_msg("Sending back: ~p", [pack_message(Data)]),
    yaws_api:websocket_send(State#state.websocket, pack_message(Data)),
    {noreply, State}.

%% This is where our websocket comms will come in
handle_info({ok, WebSocket}, State) ->
    error_logger:info_msg("Websocket started on ~p~n", [self()]),
    chloe_channel_store:subscribe("/all", self()),
    SessionId = perform_session_handshake(WebSocket),
    {noreply, State#state{websocket = WebSocket, session_id = SessionId}};
handle_info({tcp, _WebSocket, DataFrame}, State) ->
    Data = yaws_api:websocket_unframe_data(DataFrame),
    Message = unpack_message(Data),
    error_logger:info_msg("Got data from WebSocket: ~p~n", [Message]),
    send_to_ruby(Message),
    {noreply, State};
handle_info({tcp_closed, _WebSocket}, State) ->
    {stop, ok, State};
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

send_to_all_subscribers(Data) ->
    {ok, Subscribers} = chloe_channel_store:fetch_subscribers("/all"),
    lists:foreach(
        fun(Pid) ->
            chloe_websocket:send(Pid, Data)
        end,
        Subscribers).

send_to_ruby(Data) ->
    httpc:request(post, {"http://localhost:4567/updates",
                         [],
                         "text/plain",
                         Data},
                  [], [{sync, false}]).

perform_session_handshake(WebSocket) ->
    {ok, SessionIdInt} = chloe_session_manager:create(self()),
    SessionId = integer_to_list(SessionIdInt),
    Message = chloe_socketio_protocol:pack(handshake, SessionId),
    yaws_api:websocket_send(WebSocket, Message),
    SessionId.

pack_message(Data) ->
    chloe_socketio_protocol:pack(message, "", Data).

%% TODO: Unpack according to spec at https://github.com/LearnBoost/Socket.IO-node
unpack_message(Data) ->
    {ok, message, _, Message} = chloe_socketio_protocol:parse(Data),
    Message.
