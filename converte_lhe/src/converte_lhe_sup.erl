-module(converte_lhe_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		{dao_server, {dao_server, start_link, []}, permanent, 1000, worker, [dao_server]}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
