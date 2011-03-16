-module(chloe_session).

-behaviour(gen_server).
-include_lib("./chloe.hrl").

%% API
-export([
         start_link/1,
         send_to_server/2,
         subscribe/2,
         send_to_browser/3,
         retrieve_messages/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {transport_pid, messages}).

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

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([TransportPid]) ->
    chloe_channel_store:subscribe("/all", self()),
    {ok, #state{transport_pid = TransportPid, messages = []}}.

handle_call(retrieve_messages, _From, State) ->
    Messages = State#state.messages,
    error_logger:info_msg("Old State was ~p~n", [State]),
    NewState = State#state{messages= [] },
    error_logger:info_msg("New State is ~p~n", [NewState]),
    {reply, Messages, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_to_server, Data}, State) ->
    send_data_to_server(Data),
    {noreply, State};
handle_cast({subscribe, Channel}, State) ->
    error_logger:info_msg("Subscribing to channel ~p~n", [Channel]),
    chloe_channel_store:subscribe(Channel, self()),
    {noreply, State};
handle_cast({send_to_browser, [Channel, Data]}, State) ->
    NewState = case State#state.transport_pid of
                    undefined -> store_message_for_later(Channel, Data, State);
                    _         -> send_message_to_browser(Channel, Data, State)
                end,
    {noreply, NewState}.

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
    {ok, Origin} = application:get_env(chloe, application_server),
    {ok, Path} = application:get_env(chloe, application_path),
    Url = string:concat(Origin, Path),
    httpc:request(post, {Url,
                         [],
                         "text/plain",
                         Data},
                  [], [{sync, false}]).

store_message_for_later(Channel, Data, State) ->
    Messages = [#message{channel=Channel, data=Data} | State#state.messages],
    State#state{messages=Messages}.

send_message_to_browser(Channel, Data, State) ->
    gen_server:cast(State#state.transport_pid, {send, [Channel, Data]}),
    State.
