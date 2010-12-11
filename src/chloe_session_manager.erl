-module(chloe_session_manager).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         create/1
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

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    ets:new(?TABLE_ID, [protected, named_table]),
    {ok, #state{next_session_id=1}}.

handle_call({create, Pid}, _From, State) ->
    SessionId = State#state.next_session_id,
    ets:insert(?TABLE_ID, {SessionId, Pid}),
    NewState = State#state{next_session_id=SessionId + 1},
    {reply, {ok, SessionId}, NewState}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Info, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

