-module(chloe_websocket_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_child/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?SERVER, []).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    Websocket = {chloe_websocket, {chloe_websocket, start_link, []},
                 temporary, brutal_kill, worker, [chloe_websocket]},
    Children = [Websocket],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
