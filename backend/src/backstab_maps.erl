-module(backstab_maps).
-export([load/1, random/0, create_random/1]).
-include("backstab.hrl").

load(_MapId) ->
  to_front(random()).

create_random(Num) ->
  {ok, Store} = riakc_pb_socket:start_link("127.0.0.1", 8087),
  ok = create_random(Store, Num).

create_random(_Store, 0) ->
  ok;
create_random(Store, Num) ->
  Object = riakc_obj:new(<<"maps">>, uuid:v4(), bert:encode(random())),
  riakc_pb_socket:put(Store, Object),
  create_random(Num - 1).

random() ->
  Map = digraph:new(),
  PlanetsNum = random:uniform(?PLANETS_NUM_RAND_MAX) + ?PLANETS_NUM_FIX,
  Planets = generate_planets(PlanetsNum),
  [digraph:add_vertex(Map, P) || P <- Planets],
  generate_routes(Map),
  Map.

%% Map

to_front(Map) ->
  Routes = [to_route(Map, E) || E <- digraph:edges(Map)],
  [{planets, digraph:vertices(Map)}, {routes, Routes}].

to_route(Map, Edge) ->
  {_, Source, Dest, _} = digraph:edge(Map, Edge),
  #route{from = Source#planet.id, to=Dest#planet.id}.

%% Routes generator

generate_routes(Map) ->
  Planets = digraph:vertices(Map),
  generate_routes(Planets, Planets, Map).

generate_routes([From , To | []], AllPlanets, Map) ->
  generate_routes_for(From, To, lists:subtract(AllPlanets, [From, To]), Map);
generate_routes([From , To | Planets], AllPlanets, Map) ->
  generate_routes_for(From, To, lists:subtract(AllPlanets, [From, To]), Map),
  generate_routes([To | Planets], AllPlanets, Map).

generate_routes_for(From, To, Planets, Map) ->
  add_route(From, To, Map),
  random_routes(From, random:uniform(?ROUTES_MAX_COUNT - 1), Planets, Map).

random_routes(_From, 0, _Planets, _Map) ->
  ok;
random_routes(From, Count, Planets, Map) ->
  add_route(From, lists:nth(random:uniform(length(Planets)), Planets), Map),
  random_routes(From, Count - 1, Planets, Map).

add_route(From, To, Map) ->
  case lists:member(To, lists:merge(digraph:in_neighbours(Map, From), digraph:out_neighbours(Map, From))) of
    true -> ok;
    false -> digraph:add_edge(Map, From, To)
  end.

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
  Capacity = random:uniform(?PLANET_MAX_CAPACITY_FACTOR) * ?PLANET_CAPACITY_FACTOR + ?PLANET_CAPACITY_CONST,
  random_planet(UserId, Uid, Capacity).

random_planet(undefined, Uid, Capacity) ->
  #planet{id = Uid, capacity = Capacity, type = <<"ground">>};
random_planet(UserId, Uid, Capacity) ->
  Quantity = random:uniform(?PLANET_MAX_QUANTITY_FACTOR) * ?PLANET_QUANTITY_FACTOR,
  #planet{id = Uid, capacity = Capacity, type = <<"ground">>, user_id = UserId, quantity = Quantity}.

random_user() ->
  random_user(random:uniform(15)).

random_user(Seed) when Seed < 3 ->
  <<"1">>;
random_user(Seed) when Seed > 13 ->
  <<"2">>;
random_user(_) ->
  undefined.

