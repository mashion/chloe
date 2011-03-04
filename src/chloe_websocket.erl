%% To test run the following in chrome:
%% ws = new WebSocket("ws://localhost:8888/updates");
%% ws.onmessage = function (m) { console.log(m.data); };
%% ws.send("Hey dude");

-module(chloe_websocket).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         send/3
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

send(Pid, Channel, Data) ->
    gen_server:cast(Pid, {send, [Channel, Data]}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, [Channel, Data]}, State) ->
    Packed = chloe_message:pack(#message{data=Data, channel=Channel}),
    error_logger:info_msg("Sending back: ~p", [Packed]),
    yaws_api:websocket_send(State#state.websocket, Packed),
    {noreply, State}.

%% This is where our websocket comms will come in
handle_info({ok, WebSocket}, State) ->
    error_logger:info_msg("Websocket started on ~p~n", [self()]),
    SessionId = perform_session_handshake(WebSocket),
    {noreply, State#state{websocket = WebSocket,
                          session_id = SessionId}};
handle_info({tcp, _WebSocket, DataFrame}, State) ->
    error_logger:info_msg("Raw DataFrame: ~p~n", [DataFrame]),
    Data = yaws_websocket_unframe_data_patched(DataFrame),
    Message = chloe_message:unpack(Data),
    handle_message(Message, State),
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

perform_session_handshake(_WebSocket) ->
    {ok, SessionId} = chloe_session_manager:create(self()),
%    Message = chloe_socketio_protocol:pack(handshake, SessionId),
%    yaws_api:websocket_send(WebSocket, Message),
    SessionId.

session_pid(SessionId) ->
    {ok, SessionPid} = chloe_session_manager:fetch_pid(list_to_integer(SessionId)),
    SessionPid.

handle_message(Message, State) ->
    case Message#message.type of
        "channel-subscribe" -> handle_channel_subscribe_message(Message, State);
        _ -> handle_data_message(Message, State)
    end.

handle_channel_subscribe_message(Message, State) ->
    chloe_session:subscribe(session_pid(State#state.session_id),
                            Message#message.channel).

handle_data_message(Message, State) ->
    error_logger:info_msg("Got data from WebSocket: ~p~n", [Message#message.data]),
    chloe_session:send_to_server(session_pid(State#state.session_id),
                                 Message#message.data).

%%--------------------------------------------------------------------
%% Patched functions from yaws_websocket.erl
%%--------------------------------------------------------------------
yaws_websocket_unframe_data_patched(DataFrames) ->
    <<Type, _/bitstring>> = DataFrames,
    case Type of
    T when (T =< 127) ->
        %% {ok, FF_Ended_Frame} = re:compile("^.(.*)\\xFF(.*?)", [ungreedy, dotall]),
        FF_Ended_Frame = {re_pattern,2,0,
                  <<69,82,67,80,79,0,0,0,20,2,0,0,5,0,0,0,2,0,0,0,
                                0,0,255,2,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,93,0,27,25,12,94,0,7,0,1,57,12,84,
                                0,7,27,255,94,0,7,0,2,56,12,84,0,7,84,0,27,0>>},
        {match, [Data, _NextFrame]} =
        re:run(DataFrames, FF_Ended_Frame,
               [{capture, all_but_first, binary}]),
        Data;
    _ -> %% Type band 16#80 =:= 16#80
        {Length, LenBytes} = yaws_websockets_unpack_length(DataFrames, 0, 0),
        <<_, _:LenBytes/bytes, Data:Length/bytes,
         _NextFrame/bitstring>> = DataFrames,
        Data
    end.

yaws_websockets_unpack_length(Binary, LenBytes, Length) ->
    <<_, _:LenBytes/bytes, B, _/bitstring>> = Binary,
    B_v = B band 16#7F,
    NewLength = (Length * 128) + B_v,
    case B band 16#80 of
16#80 ->
        yaws_websockets_unpack_length(Binary, LenBytes + 1, NewLength);
    0 ->
        {NewLength, LenBytes + 1}
    end.
