
-module(chloe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WORKER(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(SUPERVISOR(I, Child), {I, {I, start_link, []}, permanent, 5000, supervisor, [I, Child]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Yaws = ?WORKER(chloe_yaws),
    ChannelStore = ?WORKER(chloe_channel_store),
    SessionManager = ?WORKER(chloe_session_manager),
    WebSocketSup = ?SUPERVISOR(chloe_websocket_sup, chloe_websocket),
    JsonpStreamSup = ?SUPERVISOR(chloe_jsonp_stream_sup, chloe_jsonp_stream),
    SessionSup = ?SUPERVISOR(chloe_session_sup, chloe_session),
    Children = [Yaws, ChannelStore, SessionManager, WebSocketSup,
                JsonpStreamSup, SessionSup],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

