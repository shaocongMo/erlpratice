-module(erldanmu_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, start_child/2, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Mod) ->
    start_child(Mod, Mod, []).

start_child(Mod, ProcessName) ->
    start_child(Mod, ProcessName, []).

start_child(Mod, ProcessName, Args) ->
    supervisor:start_child(?MODULE, 
       {ProcessName,
        {Mod, start_link, Args},
         permanent, brutal_kill, worker, [Mod]}).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(m_timer, worker)]} }.

