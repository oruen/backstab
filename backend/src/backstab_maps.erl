-module(backstab_maps).
-export([load/1, random/0]).
-include("backstab.hrl").

load(_MapId) ->
  random().

random() ->
  UserPlanets = [random_planet(<<"1">>),
                 random_planet(<<"1">>),
                 random_planet(<<"2">>),
                 random_planet(<<"2">>)],
  PlanetsNum = random:uniform(3) + 2,
  Planets = generate_planets(PlanetsNum),
  Routes = generate_routes(Planets),
  [{planets, UserPlanets ++ Planets}, {routes, Routes}].

%% Routes generator

generate_routes(Planets) ->
  generate_routes(Planets, []).

generate_routes([From , To | []], Acc) ->
  [#route{from = From#planet.id, to = To#planet.id} | Acc];
generate_routes([From , To | Planets], Acc) ->
  generate_routes([To | Planets], [#route{from = From#planet.id, to = To#planet.id} | Acc]).


%% Planets generator

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
  Uid = list_to_binary(uuid:to_string(uuid:v4())),
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

