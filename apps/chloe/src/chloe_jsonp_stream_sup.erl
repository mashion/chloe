-module(chloe_jsonp_stream_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_child/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Socket, Message) ->
    supervisor:start_child(?SERVER, [Socket, Message]).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    JsonpStream = {chloe_jsonp_stream, {chloe_jsonp_stream, start_link, []},
                   temporary, brutal_kill, worker, [chloe_session]},
    Children = [JsonpStream],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
