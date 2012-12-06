-define(PRINT(Var), io:format("~p:~p: ~p = ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(PLANETS_NUM_FIX, 10).
-define(PLANETS_NUM_RAND_MAX, 6).
-define(ROUTES_MAX_COUNT, 3).
-define(PLANET_MAX_CAPACITY_FACTOR, 7).
-define(PLANET_CAPACITY_FACTOR, 5).
-define(PLANET_CAPACITY_CONST, 15).
-define(PLANET_MAX_QUANTITY_FACTOR, 3).
-define(PLANET_QUANTITY_FACTOR, 5).

-record(planet, {id        :: binary(),
                 type      :: binary(),
                 user_id   = false,
                 quantity  = 0 :: integer(),
                 capacity  = 0 :: integer()}).

-record(planet_system, {id :: binary(),
                        user_id :: binary() | undefined,
                        map}).

-record(route, {from :: binary(),
                to   :: binary()}).

-record(user, {id :: binary()}).
