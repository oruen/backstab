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
    PopulatedPlanets = [Defender#planet{user_id = PlanetSystem#planet_system.user_id}] ++
                       lists:delete(Attacker, lists:delete(Defender, Planets)) ++
                       [Attacker#planet{user_id = UserId}],
    PopulatedMap = [{planets, PopulatedPlanets} | lists:delete({planets, Planets}, Map)],
    PopulatedPlanetSystem = PlanetSystem#planet_system{map = PopulatedMap},
    State = [{map, PopulatedPlanetSystem},
             {riak, RiakPid},
             {players, [[{id, UserId}, {socket, UserSocket}]]}],
    erlang:start_timer(100, UserSocket, {send, map, PopulatedPlanetSystem}),
    {ok, State}.

stop() ->
    ok.

handle_call(_Cmd, _From, State) ->
    {ok, State}.

handle_cast({goto, [From, To]}, State) ->
    % TODO Validate route
    {_, Players} = lists:keyfind(players, 1, State),
    lists:map(fun(P) ->
        {_, Socket} = lists:keyfind(socket, 1, P),
        erlang:start_timer(0, Socket, {send, goto, [From, To]})
    end, Players),
    {noreply, State};
handle_cast(_Cmd, State) ->
    {noreply, State}.

% Interrupt battle on gamer disconnect.
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Cmd, State) ->
    {noreply, State}.

terminate(_Type, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
