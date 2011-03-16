-module(chloe_session_manager).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         create/1,
         fetch_pid/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_ID, ?MODULE).

-record(state, {next_session_id}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(Pid) ->
    gen_server:call(?SERVER, {create, Pid}).

fetch_pid(SessionId) when is_list(SessionId) ->
    fetch_pid(list_to_integer(SessionId));
fetch_pid(SessionId) ->
    case ets:lookup(?TABLE_ID, SessionId) of
        [{SessionId, SessionPid}] -> {ok, SessionPid};
        []                        -> {error, session_not_found}
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    ets:new(?TABLE_ID, [protected, named_table]),
    {ok, #state{next_session_id=1}}.

handle_call({create, Pid}, _From, State) ->
    SessionId = State#state.next_session_id,
    {ok, SessionPid} = chloe_session_sup:start_child(Pid),
    ets:insert(?TABLE_ID, {SessionId, SessionPid}),
    NewState = State#state{next_session_id=SessionId + 1},
    {reply, {ok, integer_to_list(SessionId)}, NewState}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Info, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

