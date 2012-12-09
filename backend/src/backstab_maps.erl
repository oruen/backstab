-module(backstab_maps).
-export([load/2, random/0, create_random/1, to_front/1, store/3]).
-include("backstab.hrl").

load(MapId, RiakPid) ->
  {ok, O} = riakc_pb_socket:get(RiakPid, <<"maps">>, MapId),
  Map = riakc_obj:get_value(O),
  {ok, bert:decode(Map)}.

create_random(Num) ->
  random:seed(erlang:now()),
  {ok, Store} = riakc_pb_socket:start_link("127.0.0.1", 8087),
  ok = create_random(Store, Num).

create_random(_Store, 0) ->
  ok;
create_random(Store, Num) ->
  store(random(), false, Store),
  create_random(Store, Num - 1).

store(Map, UserId, Store) ->
  Id = list_to_binary(uuid:to_string(uuid:v4())),
  Object = riakc_obj:new(<<"maps">>, Id, bert:encode(#planet_system{user_id = UserId, map = to_front(Map), id = Id})),
  riakc_pb_socket:put(Store, Object).

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
  Planets = digraph:vertices(Map),
  %[lists:nth(1, Planets)] ++ lists:sublist(Planets, 2, length(Planets) - 1) ++ [lists:last(Planets)],
  [{planets, Planets}, {routes, Routes}].

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
  Uid = list_to_binary(uuid:to_string(uuid:v4())),
  Quantity = random:uniform(?PLANET_MAX_QUANTITY_FACTOR) * ?PLANET_QUANTITY_FACTOR,
  Capacity = random:uniform(?PLANET_MAX_CAPACITY_FACTOR) * ?PLANET_CAPACITY_FACTOR + ?PLANET_CAPACITY_CONST,
  #planet{id = Uid, capacity = Capacity, type = <<"ground">>, quantity = Quantity}.
