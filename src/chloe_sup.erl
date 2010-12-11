
-module(chloe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Yaws = ?CHILD(chloe_yaws, worker),
    ChannelStore = ?CHILD(chloe_channel_store, worker),
    SessionManager = ?CHILD(chloe_session_manager, worker),
    WebSocket = {chloe_websocket_sup, {chloe_websocket_sup, start_link, []},
                 permanent, 5000, supervisor, [chloe_websocket]},
    Children = [Yaws, ChannelStore, SessionManager, WebSocket],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

