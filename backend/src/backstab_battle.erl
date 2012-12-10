-module(backstab_battle).
-behaviour(gen_server).
-include("backstab.hrl").

-export([start_link/1, stop/0, init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({MapId, UserId, UserSocket}) ->
    link(UserSocket),
    process_flag(trap_exit, true),
    {ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, PlanetSystem} = backstab_maps:load(MapId, RiakPid),
    Map = PlanetSystem#planet_system.map,
    {planets, Planets} = lists:keyfind(planets, 1, Map),
    Defender = lists:nth(1, Planets),
    Attacker = lists:last(Planets),
    Graph = build_digraph(Map),
    digraph:add_vertex(Graph, Defender#planet.id, Defender#planet{user_id = PlanetSystem#planet_system.user_id}),
    digraph:add_vertex(Graph, Attacker#planet.id, Attacker#planet{user_id = UserId}),
    State = [{map, Graph},
             {riak, RiakPid},
             {players, [[{id, UserId}, {socket, UserSocket}]]}],
    PopulatedPlanetSystem = PlanetSystem#planet_system{map = backstab_maps:to_front(Graph)},
    erlang:start_timer(100, UserSocket, {send, map, PopulatedPlanetSystem}),
    {ok, State}.

build_digraph(Map) ->
    Graph = digraph:new(),
    [{planets, Planets}, {routes, Routes}] = Map,
    [digraph:add_vertex(Graph, Planet#planet.id, Planet) || Planet <- Planets],
    [digraph:add_edge(Graph, Route#route.from, Route#route.to) || Route <- Routes],
    Graph.

stop() ->
    ok.

handle_call(_Cmd, _From, State) ->
    {ok, State}.

handle_cast({goto, [From, To]}, State) ->
    {_, Map} = lists:keyfind(map, 1, State),
    case backstab_maps:planets_connected(From, To, Map) of
        true ->
            {_, PlanetFrom} = digraph:vertex(Map, From),
            digraph:add_vertex(Map, From, PlanetFrom#planet{quantity = 0}),
            send_all({send, population, {From, 0}}, State),
            send_all({send, goto, [From, To]}, State),
            Quantity = PlanetFrom#planet.quantity,
            UserId = PlanetFrom#planet.user_id,
            erlang:start_timer(1000, self(), {Quantity, UserId, To});
        false -> ok
    end,
    {noreply, State};
handle_cast(_Cmd, State) ->
    {noreply, State}.

send_all(Msg, State) ->
    {_, Players} = lists:keyfind(players, 1, State),
    lists:map(fun(P) ->
        {_, Socket} = lists:keyfind(socket, 1, P),
        erlang:start_timer(0, Socket, Msg)
    end, Players).

% Interrupt battle on gamer disconnect.
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_info({timeout, _Ref, {Quantity, UserId, Dest}}, State) ->
    {_, Map} = lists:keyfind(map, 1, State),
    {_, Planet} = digraph:vertex(Map, Dest),
    Quantity1 = case UserId == Planet#planet.user_id of
        true ->
            Quantity + Planet#planet.quantity;
        false ->
            Planet#planet.quantity - Quantity
    end,
    Planet1 = Planet#planet{quantity = Quantity1},
    digraph:add_vertex(Map, Dest, Planet1),
    send_all({send, population, {Dest, Quantity1}}, State),
    {noreply, State};
handle_info(_Cmd, State) ->
    {noreply, State}.

terminate(_Type, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
