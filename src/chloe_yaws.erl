-module(chloe_yaws).

%% API
-export([
         start_link/0,
         run/0
       ]).

%% Supervisor module
-define(SUPERVISOR, chloe_sup).

start_link() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    GconfList = [{id, Id}],
    Docroot = "./public",
    SconfList = [{port, 8888},
                 {servername, "foobar"},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot},
                 {appmods, [{"/chloe/websocket", chloe_yaws_updates},
                            {"/send", chloe_yaws_send}]}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(?SUPERVISOR, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.

