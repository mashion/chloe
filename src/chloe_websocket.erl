%% To test run the following in chrome:
%% ws = new WebSocket("ws://localhost:8888/updates");
%% ws.onmessage = function (m) { console.log(m.data); };
%% ws.send("Hey dude");

-module(chloe_websocket).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

%% This is where our websocket comms will come in
handle_info({ok, _WebSocket}, State) ->
    {noreply, State};
handle_info({tcp, WebSocket, DataFrame}, State) ->
    Data = yaws_api:websocket_unframe_data(DataFrame),
    error_logger:info_msg("Got data from WebSocket: ~p~n", [Data]),
    yaws_api:websocket_send(WebSocket, Data),
    {noreply, State};
handle_info({tcp_closed, _WebSocket}, State) ->
    {stop, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
