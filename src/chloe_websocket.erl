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

-record(state, {websocket}).

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
    yaws_api:websocket_send(State#state.websocket, Data),
    {noreply, State}.

%% This is where our websocket comms will come in
handle_info({ok, WebSocket}, State) ->
    error_logger:info_msg("Websocket started on ~p~n", [self()]),
    chloe_channel_store:subscribe("/all", self()),
    {noreply, State#state{websocket = WebSocket}};
handle_info({tcp, _WebSocket, DataFrame}, State) ->
    Data = yaws_api:websocket_unframe_data(DataFrame),
    error_logger:info_msg("Got data from WebSocket: ~p~n", [Data]),
    send_to_all_subscribers(Data),
    {noreply, State};
handle_info({tcp_closed, _WebSocket}, State) ->
    {stop, ok, State}.

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
