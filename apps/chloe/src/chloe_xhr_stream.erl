-module(chloe_xhr_stream).

-behaviour(gen_server).
-include_lib("./chloe.hrl").

%% API
-export([
         start_link/2,
         send/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket, message, yaws_pid}).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link(Socket, Message) ->
    gen_server:start_link(?MODULE, [Socket, Message], []).

send(Pid, Channel, Data) ->
    gen_server:cast(Pid, {send, [Channel, Data]}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Socket, Message]) ->
    {ok, #state{socket = Socket, message = Message}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, [Messages]}, State) ->
    Packed = chloe_message:pack(Messages),
    yaws_api:stream_process_deliver_final_chunk(State#state.socket, Packed),
    yaws_api:stream_process_end(State#state.socket, State#state.yaws_pid),
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({ok, YawsPid}, State) ->
    Message = State#state.message,
    {ok, SessionPid} = chloe_session_manager:fetch_pid(Message#message.session_id),
    %% Now that Yaws is ready, we attach ourselves to our session.
    chloe_session:attach_transport_pid(SessionPid, self()),
    {noreply, State#state{yaws_pid = YawsPid}};
handle_info({discard, YawsPid}, State) ->
    yaws_api:stream_process_end(State#state.socket, YawsPid),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
