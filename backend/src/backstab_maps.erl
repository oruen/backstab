-module(backstab_maps).
-export([load/1, random/0]).
-include("backstab.hrl").

load(_MapId) ->
  random().

random() ->
  Map = digraph:new(),
  UserPlanets = [random_planet(<<"1">>),
                 random_planet(<<"1">>),
                 random_planet(<<"2">>),
                 random_planet(<<"2">>)],
  PlanetsNum = random:uniform(3) + 2,
  Planets = generate_planets(PlanetsNum),
  [digraph:add_vertex(Map, P) || P <- UserPlanets],
  [digraph:add_vertex(Map, P) || P <- Planets],
  generate_routes(Map),
  to_front(Map).

%% Map

to_front(Map) ->
  Routes = [to_route(Map, E) ||E <- digraph:edges(Map)],
  [{planets, digraph:vertices(Map)}, {routes, Routes}].

to_route(Map, Edge) ->
  {_, Source, Dest, _} = digraph:edge(Map, Edge),
  #route{from = Source#planet.id, to=Source#planet.id}.

%% Routes generator

generate_routes(Map) ->
  Planets = digraph:vertices(Map),
  generate_routes(Planets, Planets, Map).

generate_routes([From , To | []], AllPlanets, Map) ->
  digraph:add_edge(Map, From, To),
  Map;
generate_routes([From , To | Planets], AllPlanets, Map) ->
  %RandomRoutes = random_route(AllPlanets, [From, To]),
  digraph:add_edge(Map, From, To),
  generate_routes([To | Planets], AllPlanets, Map).


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

