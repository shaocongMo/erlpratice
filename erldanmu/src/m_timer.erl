%% @author Administrator
%% @doc @todo Add description to m_timer.
-module(m_timer).
-behaviour(gen_server).

-include("common.hrl").

-define(CLOCK, 100).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
         get_time_offset/0,
         set_time_offset/1,
         add_time_offset/1,
         now/0, 
         now_seconds/0, 
         now_milseconds/0, 
         cpu_time/0, 
         start_link/0, 
         start/1, 
         info/0]).

%% ====================================================================
%% API functions
%% ====================================================================

get_time_offset() ->
    Time = gen_server:call(?MODULE, {get_time_offset}), 
    Time.

set_time_offset(Seconds) ->
    gen_server:cast(?MODULE, {set_time_offset, Seconds}),   
    ok.

add_time_offset(Seconds) ->
    gen_server:cast(?MODULE, {add_time_offset, Seconds}),   
    ok.

now() -> 
    [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
    Now.

now_seconds()->
    [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
    {MegaSecs, Secs, _MicroSecs} = Now, 
    MegaSecs * 1000000 + Secs.

now_milseconds() ->
    [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
    {M, S, Ms} = Now,
    (M * 1000000000000 + S * 1000000 + Ms) div 1000.

cpu_time() -> 
    [{timer, {_, Wallclock_Time_Since_Last_Call}}] = ets:lookup(ets_timer, timer),
    Wallclock_Time_Since_Last_Call.

info() ->
    [
     ets:info(ets_timer), 
     ets:tab2list(ets_timer)
    ].

start(Sup) ->
    supervisor:start_child(Sup, ?CHILD(m_timer, worker)).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    put(dic_time_offset, 0),
    ets:new(ets_timer, [set, protected, named_table]),
    ets:insert(ets_timer, {timer, {os:timestamp(), 0}}),
    erlang:send_after(?CLOCK, self(), {event, clock}),  
    {ok, []}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call({get_time_offset}, _,  State) ->
    Offset = get(dic_time_offset),
    {reply, Offset, State};

handle_call(_Request, _From, State) ->
    {reply, State, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({set_time_offset, Seconds}, State) ->
    put(dic_time_offset, Seconds),
    {noreply, State};

handle_cast({add_time_offset, Seconds}, State) ->
    Offset = get(dic_time_offset),
    put(dic_time_offset, Offset + Seconds),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({event, clock}, State) ->
    erlang:send_after(?CLOCK, self(), {event, clock}),
    {_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),    
    Offset = get(dic_time_offset),
    NewTimeCall = {MegaSecs, Secs + Offset, _MicroSecs},
    ets:insert(ets_timer, {timer, {NewTimeCall, Time_Since_Last_Call}}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


