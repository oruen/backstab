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
    Map = backstab_maps:load(MapId),
    User = #user{id = UserId},
    State = {Map, [{User, UserSocket}]},
    erlang:start_timer(0, UserSocket, {send, userinfo, User}),
    erlang:start_timer(100, UserSocket, {send, map, Map}),
    {ok, State}.

stop() ->
    ok.

handle_call(_Cmd, _From, State) ->
    {ok, State}.

handle_cast({goto, [From, To]}, State) ->
    {_, [{_, UserSocket}]} = State,
    erlang:start_timer(0, UserSocket, {send, goto, [From, To]}),
    {noreply, State};
    %send_all({goto, From, To}, State)
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
