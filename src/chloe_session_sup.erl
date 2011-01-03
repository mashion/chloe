-module(chloe_session_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_child/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(TransportPid) ->
    supervisor:start_child(?SERVER, [TransportPid]).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    Session = {chloe_session, {chloe_session, start_link, []},
               temporary, brutal_kill, worker, [chloe_session]},
    Children = [Session],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
