-module(backstab_maps).
-export([load/1, random/0]).
-include("backstab.hrl").

load(_MapId) ->
    [{planets, [
        #planet{id = <<"1">>, capacity = 30,
                type = <<"ground">>, quantity = 10, user_id = <<"1">>},
        #planet{id = <<"2">>, capacity = 40, type = <<"ground">>},
        #planet{id = <<"3">>, capacity = 50,
                type = <<"ground">>, quantity = 15, user_id = <<"2">>},
        #planet{id = <<"4">>, capacity = 50, type = <<"ground">>}]},
     {routes, [
         #route{from = <<"1">>, to = <<"2">>},
         #route{from = <<"1">>, to = <<"3">>},
         #route{from = <<"2">>, to = <<"3">>},
         #route{from = <<"3">>, to = <<"4">>}]}
   ].

random() ->
  UserPlanets = [random_planet(<<"1">>),
                 random_planet(<<"1">>),
                 random_planet(<<"2">>),
                 random_planet(<<"2">>)],
  PlanetsNum = random:uniform(10) + 8,
  Planets = generate_planets(PlanetsNum),
  [{planets, UserPlanets ++ Planets}].

generate_planets(Num) ->
  generate_planets(Num, []).

generate_planets(0, Acc) ->
  Acc;
generate_planets(Num, Acc) ->
  generate_planets(Num - 1, [random_planet() | Acc]).

random_planet() ->
  UserId = random_user(),
  random_planet(UserId).

random_planet(UserId) ->
  Uid = uuid:to_string(uuid:v4()),
  Capacity = random:uniform(7) * 5 + 15,
  random_planet(UserId, Uid, Capacity).

random_planet(undefined, Uid, Capacity) ->
  #planet{id = Uid, capacity = Capacity, type = <<"ground">>};
random_planet(UserId, Uid, Capacity) ->
  Quantity = random:uniform(3) * 5,
  #planet{id = Uid, capacity = Capacity, type = <<"ground">>, user_id = UserId, quantity = Quantity}.

random_user() ->
  random_user(random:uniform(15)).

random_user(Seed) when Seed < 3 ->
  <<"1">>;
random_user(Seed) when Seed > 13 ->
  <<"2">>;
random_user(_) ->
  undefined.

