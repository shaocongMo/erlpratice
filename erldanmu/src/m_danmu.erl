%% @author Administrator
%% @doc @todo Add description to m_danmu.
-module(m_danmu).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/5,
         start/4,
         start/2]).

-include("common.hrl").

-define(LOOP_TIME, 1000).
%% ====================================================================
%% API functions
%% ====================================================================
-record(state, {host = undefined,
                port = undefined,
                service_mod = undefined,
                room_id = undefined,
                socket = undefined
                }).


start_link(Host, Port, Mod, RoomId, ProcessName) ->
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
            case Mod:login(Socket, RoomId) of
                ok ->
                    case gen_server:start_link({local, ProcessName}, ?MODULE, [Host, Port, Mod, Socket, RoomId], []) of
                        {ok, Pid} ->
                            gen_tcp:controlling_process(Socket, Pid),
                            {ok, Pid};
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            ?PRINT("error connect to danmu server, host: ~p port: ~p~n",
                   [Host, Port])
    end.

start(Host, Port, Mod, RoomId) ->
    ProcessName = list_to_atom(lists:append([atom_to_list(Mod), "_", integer_to_list(RoomId)])),
    erldanmu_sup:start_child(?MODULE, ProcessName, [Host, Port, Mod, RoomId, ProcessName]).

start(Mod, RoomParam) ->
    Mod:open(RoomParam).

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

init([Host, Port, Mod, Socket, RoomId]) ->
    try 
        do_init(Host, Port, Mod, Socket, RoomId)
    catch 
        _ : _Reason ->
            ?PRINT("m_danmu init is error:~w",[_Reason]),
            ?PRINT("get_stacktrace:~p",[erlang:get_stacktrace()]),
            {stop, normal}
    end.

do_init(Host, Port, Mod, Socket, RoomId) ->
    erlang:send_after(?LOOP_TIME, self(), {heart}),
    State = #state{host = Host,
                   port = Port,
                   service_mod = Mod,
                   room_id = RoomId,
                   socket = Socket},
    {ok, State}.

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

handle_call(Request, _From, State) ->
    ?PRINT("call : ~p~n", [Request]),
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

handle_cast(Msg, State) ->
    ?PRINT("cast msg: ~p~n", [Msg]),
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
handle_info({heart}, #state{socket = Socket, service_mod = Mod} = State) ->
    Mod:heart(Socket),
    {noreply, State};

handle_info({tcp, Socket, Data}, #state{socket = Socket, service_mod = Mod} = State) ->
    Mod:analyze_data(list_to_binary(Data)),
    {noreply, State};

% handle_info({tcp_closed, _Socket}, #state{host = Host, port = Port, service_mod = Mod, room_id = RoomId} = State) ->
%     %% 不知道为什么有时候会断掉，怀疑是心跳问题
%     {ok, NewSocket} = gen_tcp:connect(Host, Port, []),
%     Mod:login(NewSocket, RoomId),
%     gen_tcp:controlling_process(NewSocket, self()),
%     {noreply, State#state{socket = NewSocket}};

handle_info(Info, State) ->
    ?PRINT("info: ~p~n", [Info]),
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
terminate(normal, State) ->
    Mod = State#state.service_mod,
    Mod:logout(State#state.socket),
    ?PRINT("terminate normal~n"),
    ok;

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
