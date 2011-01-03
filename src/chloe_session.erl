-module(chloe_session).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         send_to_server/2,
         send_to_browser/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {transport_pid}).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link(TransportPid) ->
    gen_server:start_link(?MODULE, [TransportPid], []).

send_to_server(Pid, Data) ->
    gen_server:cast(Pid, {send_to_server, Data}).

send_to_browser(Pid, Data) ->
    gen_server:cast(Pid, {send_to_browser, Data}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([TransportPid]) ->
    chloe_channel_store:subscribe("/all", self()),
    {ok, #state{transport_pid = TransportPid}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_to_server, Data}, State) ->
    send_data_to_server(Data),
    {noreply, State};
handle_cast({send_to_browser, Data}, State) ->
    gen_server:cast(State#state.transport_pid, {send, Data}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

send_data_to_server(Data) ->
    httpc:request(post, {"http://localhost:4567/updates",
                         [],
                         "text/plain",
                         Data},
                  [], [{sync, false}]).
