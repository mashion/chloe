-module(chloe_channel_store).

-behaviour(gen_server).

-export([
         start_link/0,
         subscribe/2,
         fetch_subscribers/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_ID, ?MODULE).

-record(state, {}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(Channel, Pid) ->
    gen_server:cast(?SERVER, {subscribe, [Channel, Pid]}).

fetch_subscribers(Channel) ->
    gen_server:call(?SERVER, {fetch_subscribers, Channel}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    ets:new(?TABLE_ID, [public, named_table]),
    {ok, #state{}}.

handle_call({fetch_subscribers, Channel}, _From, State) ->
    Response = subscribers_for(Channel),
    {reply, Response, State}.

handle_cast({subscribe, [Channel, Pid]}, State) ->
    OldSubscribers = case subscribers_for(Channel) of
                        {ok, Subscribers} -> Subscribers;
                        _                 -> []
                     end,
    NewSubscribers = add_subscriber(Pid, OldSubscribers),
    ets:insert(?TABLE_ID, {Channel, NewSubscribers}),
    {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Info, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

add_subscriber(NewSubscriber, Subscribers) ->
    [NewSubscriber | lists:delete(NewSubscriber, Subscribers)].

subscribers_for(Channel) ->
    case ets:lookup(?TABLE_ID, Channel) of
        [{Channel, Subscribers}] -> {ok, Subscribers};
        []                       -> {error, channel_not_found}
    end.
