-module(chloe).

%% API
-export([
         start/0,
         stop/0
        ]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start() ->
    ensure_started(inets),
    application:start(chloe).

stop() ->
    application:stop(chloe).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
