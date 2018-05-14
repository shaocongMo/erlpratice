%%%-------------------------------------------------------------------
%% @doc erlnotice top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlnotice_sup).

-behaviour(supervisor).

-include ("erlnotice.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, get_children()} }.

%%====================================================================
%% Internal functions
%%====================================================================
get_children() ->
	case application:get_env(mail) of
		{ok, MailArgs} ->
			MailChild = child(erlnotice_mail, [proplists:get_value(account, MailArgs),
											   proplists:get_value(password, MailArgs),
											   proplists:get_value(smtp, MailArgs)]),
			Children = [MailChild];
		_ ->
			Children = []
	end,
	Children.

child(Mod, Args) ->
	{Mod, {Mod, start_link, Args}, permanent, brutal_kill, worker, [Mod]}.