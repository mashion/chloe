-module(chloe_session).

-behaviour(gen_server).
-include_lib("./chloe.hrl").

%% API
-export([
         start_link/1,
         send_to_server/2,
         subscribe/2,
         send_to_browser/3,
         retrieve_messages/1,
         attach_transport_pid/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {transport_pid, messages, failed_health_checks}).

-define(TIMEOUT, 13 * 1000).        % Primes are fun
-define(HEALTH_CHECK_THRESHOLD, 4). % Five fails == death

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link(TransportPid) ->
    gen_server:start_link(?MODULE, [TransportPid], []).

send_to_server(Pid, Data) ->
    gen_server:cast(Pid, {send_to_server, Data}).

subscribe(Pid, Channel) ->
    gen_server:cast(Pid, {subscribe, Channel}).

send_to_browser(Pid, Channel, Data) ->
    gen_server:cast(Pid, {send_to_browser, [Channel, Data]}).

retrieve_messages(Pid) ->
    gen_server:call(Pid, retrieve_messages).

attach_transport_pid(Pid, TransportPid) ->
    gen_server:cast(Pid, {attach_transport_pid, TransportPid}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([TransportPid]) ->
    chloe_channel_store:subscribe("/all", self()),
    {ok, #state{transport_pid = TransportPid, messages = [], failed_health_checks = 0}, ?TIMEOUT}.

handle_call(retrieve_messages, _From, State) ->
    Messages = State#state.messages,
    error_logger:info_msg("Old State was ~p~n", [State]),
    NewState = State#state{messages= [] },
    error_logger:info_msg("New State is ~p~n", [NewState]),
    {reply, Messages, NewState, ?TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?TIMEOUT}.

handle_cast({send_to_server, Data}, State) ->
    send_data_to_server(Data),
    {noreply, State, ?TIMEOUT};
handle_cast({subscribe, Channel}, State) ->
    error_logger:info_msg("Subscribing to channel ~p~n", [Channel]),
    chloe_channel_store:subscribe(Channel, self()),
    {noreply, State, ?TIMEOUT};
handle_cast({send_to_browser, [Channel, Data]}, State) ->
    %% TODO (trotter): Handle case where we have a pid but
    %%                 it's no longer active.
    NewState = case is_transport_available(State#state.transport_pid) of
                    true  -> send_message_to_browser(Channel, Data, State);
                    _     -> store_message_for_later(Channel, Data, State)
                end,
    {noreply, NewState, ?TIMEOUT};
handle_cast({attach_transport_pid, TransportPid}, State) ->
    Messages = State#state.messages,
    case Messages of
        [] -> NewState = State;
        _  -> NewState = State#state{messages = []},
              gen_server:cast(TransportPid, {send, [Messages]})
    end,
    {noreply, NewState#state{transport_pid = TransportPid}, ?TIMEOUT}.

handle_info(timeout, State) ->
    case check_transport_health(State) of
        dead      -> {stop, normal, State};
        unhealthy -> {noreply,
                      State#state{failed_health_checks=State#state.failed_health_checks + 1},
                      ?TIMEOUT};
        ok        -> {noreply,
                      State#state{failed_health_checks=0},
                      ?TIMEOUT}
    end;
% TODO (trotter): We're getting weird messages from the app server,
%                 need to investigate.
handle_info(_Info, State) ->
    {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

check_transport_health(State) ->
    case is_transport_available(State#state.transport_pid) of
        true -> ok;
        _ -> case State#state.failed_health_checks > ?HEALTH_CHECK_THRESHOLD of
                true  -> dead;
                _     -> unhealthy
             end
    end.

send_data_to_server(Data) ->
    {ok, Url} = application:get_env(chloe, application_server_url),
    httpc:request(post, {Url,
                         [],
                         "text/plain",
                         Data},
                  [], [{sync, false}]).

store_message_for_later(Channel, Data, State) ->
    Messages = [#message{channel=Channel, data=Data} | State#state.messages],
    State#state{messages=Messages, transport_pid=undefined}.

send_message_to_browser(Channel, Data, State) ->
    Messages = [#message{channel=Channel, data=Data}],
    gen_server:cast(State#state.transport_pid, {send, [Messages]}),
    State.

is_transport_available(TransportPid) ->
    case TransportPid of
        undefined -> false;
        _         -> is_process_alive(TransportPid)
    end.
