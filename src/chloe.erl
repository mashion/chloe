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
    ok = application:start(inets),
    application:start(chloe).

stop() ->
    ok = application:stop(chloe),
    application:stop(inets).
