-module(backstab_battle_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 1, 10},
    [{battle,
        {backstab_battle, start_link, []},
        temporary, 5000, worker, [backstab_battle]}
    ]}}.

