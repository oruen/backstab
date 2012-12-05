-define(PRINT(Var), io:format("~p:~p: ~p = ~p~n", [?MODULE, ?LINE, ??Var, Var])).

-record(planet, {id        :: binary(),
                 type      :: binary(),
                 user_id   = false,
                 quantity  = 0 :: integer(),
                 capacity  = 0 :: integer()}).

-record(route, {from :: binary(),
                to   :: binary()}).

-record(user, {id :: binary()}).
