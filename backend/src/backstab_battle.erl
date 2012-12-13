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
    digraph:add_vertex(Graph, Defender#planet.id, Defender#planet{quantity = ?PLAYER_START_POPULATION, user_id = PlanetSystem#planet_system.user_id}),
    digraph:add_vertex(Graph, Attacker#planet.id, Attacker#planet{quantity = ?PLAYER_START_POPULATION, user_id = UserId}),
    State = [{map, Graph},
             {riak, RiakPid},
             {players, [[{id, UserId}, {socket, UserSocket}]]}],
    PopulatedPlanetSystem = PlanetSystem#planet_system{map = backstab_maps:to_front(Graph)},
    erlang:start_timer(100, UserSocket, {send, map, PopulatedPlanetSystem}),
    erlang:start_timer(?PLANET_GROWTH_TIMEOUT, self(), planets_growth),
    erlang:start_timer(?PLAYER_PLANET_GROWTH_TIMEOUT, self(), player_planets_growth),
    % Send planet host an invitation
    Msg = {defend, PlanetSystem#planet_system.user_id, list_to_binary(pid_to_list(self()))},
    gproc:send({p, l, PlanetSystem#planet_system.user_id}, Msg),
    {ok, State}.

build_digraph(Map) ->
    Graph = digraph:new(),
    [{planets, Planets}, {routes, Routes}] = Map,
    [digraph:add_vertex(Graph, Planet#planet.id, Planet) || Planet <- Planets],
    [digraph:add_edge(Graph, Route#route.from, Route#route.to) || Route <- Routes],
    Graph.

stop() ->
    ok.

handle_call({enter_battle, Email}, From, State) ->
    {_, Players} = lists:keyfind(players, 1, State),
    {UserSocket, _} = From,
    link(UserSocket),
    Players1 = [[{id, Email}, {socket, UserSocket}] | Players],
    State1 = lists:keystore(players, 1, State, {players, Players1}),
    {_, Map} = lists:keyfind(map, 1, State),
    erlang:start_timer(0, UserSocket, {send, map, #planet_system{user_id = Email, map = backstab_maps:to_front(Map)}}),
    {reply, ok, State1};
handle_call(_Cmd, _From, State) ->
    {noreply, ok, State}.

handle_cast({{goto, [From, To]}, Player}, State) ->
    {_, Map} = lists:keyfind(map, 1, State),
    {_, PlanetFrom} = digraph:vertex(Map, From),
    UserId = PlanetFrom#planet.user_id,
    {_, InitUserId} = lists:keyfind(<<"email">>, 1, Player),
    case UserId == InitUserId of
        true ->
            case backstab_maps:planets_connected(From, To, Map) of
                true ->
                    digraph:add_vertex(Map, From, PlanetFrom#planet{quantity = 0}),
                    send_all({send, population, [{From, 0}]}, State),
                    send_all({send, goto, [From, To]}, State),
                    Quantity = PlanetFrom#planet.quantity,
                    erlang:start_timer(1000, self(), {Quantity, UserId, To});
                false -> ok
            end;
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
handle_info({'EXIT', Pid, normal}, State) ->
    % {players, [[{id, UserId}, {socket, UserSocket}]]}
    {_, Players} = lists:keyfind(players, 1, State),
    Players1 = lists:filter(fun(Player) ->
        case Player of
            [{id, _}, {socket, Pid}] ->
                false;
            _ ->
                true
        end
    end, Players),
    State1 = lists:keystore(players, 1, State, {players, Players1}),
    case length(Players1) > 0 of
        true ->
            ?PRINT("Player left battle"),
            {noreply, State1};
        false ->
            ?PRINT("Killing time"),
            {stop, normal, State1}
    end;

handle_info({timeout, _Ref, {Quantity, UserId, Dest}}, State) ->
    {_, Map} = lists:keyfind(map, 1, State),
    {_, Planet} = digraph:vertex(Map, Dest),
    Planet1 = case UserId == Planet#planet.user_id of
        true ->
            Planet#planet{quantity = Quantity + Planet#planet.quantity};
        false ->
            if
                Planet#planet.quantity - Quantity < 1 ->
                    Planet#planet{user_id = UserId, quantity = Quantity - Planet#planet.quantity};
                true ->
                    Planet#planet{quantity = Planet#planet.quantity - Quantity}
            end
    end,
    digraph:add_vertex(Map, Dest, Planet1),
    send_all({send, planet, Planet1}, State),
    erlang:start_timer(3000, self(), check_victory),
    {noreply, State};
handle_info({timeout, _Ref, check_victory}, State) ->
    {_, Map} = lists:keyfind(map, 1, State),
    UserIds = lists:map(fun(V) ->
        {_, Planet} = digraph:vertex(Map, V),
        [Planet#planet.user_id]
    end, digraph:vertices(Map)),
    FilteredIds = lists:filter(fun(E) -> E /= false end, lists:umerge(UserIds)),
    case erlang:length(FilteredIds) of
        1 ->
            send_all({send, victory, lists:last(FilteredIds)}, State),
            {stop, normal, State};
        _Else -> {noreply, State}
    end;
handle_info({timeout, _Ref, planets_growth}, State) ->
    {_, Map} = lists:keyfind(map, 1, State),
    Planets = lists:filter(fun(Planet) ->
         Planet#planet.user_id == false andalso Planet#planet.quantity < Planet#planet.capacity
    end, lists:map(fun(V) -> {_, Planet} = digraph:vertex(Map, V), Planet end, digraph:vertices(Map))),
    Msgs = lists:map(fun(Planet) ->
        Quantity = Planet#planet.quantity + 1,
        digraph:add_vertex(Map, Planet#planet.id, Planet#planet{quantity = Quantity}),
        {Planet#planet.id, Quantity}
    end, Planets),
    send_all({send, population, Msgs}, State),
    erlang:start_timer(?PLANET_GROWTH_TIMEOUT, self(), planets_growth),
    {noreply, State};
handle_info({timeout, _Ref, player_planets_growth}, State) ->
    {_, Map} = lists:keyfind(map, 1, State),
    Planets = lists:filter(fun(Planet) ->
         Planet#planet.user_id /= false andalso Planet#planet.quantity < Planet#planet.capacity
    end, lists:map(fun(V) -> {_, Planet} = digraph:vertex(Map, V), Planet end, digraph:vertices(Map))),
    Msgs = lists:map(fun(Planet) ->
        Quantity = Planet#planet.quantity + 1,
        digraph:add_vertex(Map, Planet#planet.id, Planet#planet{quantity = Quantity}),
        {Planet#planet.id, Quantity}
    end, Planets),
    send_all({send, population, Msgs}, State),
    erlang:start_timer(?PLAYER_PLANET_GROWTH_TIMEOUT, self(), player_planets_growth),
    {noreply, State};
handle_info(_Cmd, State) ->
    {noreply, State}.

terminate(_Type, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
