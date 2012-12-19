-module(backstab_galaxy).
-include("backstab.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ~~~~~~~~~~~~~~~~~~~~
%% API Function Exports
%% ~~~~~~~~~~~~~~~~~~~~

-export([start_link/0]).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% gen_server Function Exports
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([maps/0, users/0, user/1, map/1, capture_map/2, add_user/2, add_planet_system/2]).

%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

maps() ->
    gen_server:call(server(), maps).

map(Id) ->
    gen_server:call(server(), {map, Id}).

capture_map(PlanetSystemId, UserId) ->
    gen_server:cast(server(), {capture_map, PlanetSystemId, UserId}).

users() ->
    gen_server:call(server(), users).

user(Token) ->
    gen_server:call(server(), {user, Token}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_user(Id, Userinfo) ->
    gen_server:call(server(), {add_user, Id, Userinfo}).

add_planet_system(Id, PlanetSystem) ->
    gen_server:call(server(), {add_planet_system, Id, PlanetSystem}).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% gen_server Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init(_Args) ->
    gproc:add_local_name(galaxy),
    {ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, MapKeys} = riakc_pb_socket:list_keys(RiakPid, <<"maps">>),
    Maps = lists:foldl(fun(K, Acc) ->
          {ok, O} = riakc_pb_socket:get(RiakPid, <<"maps">>, K),
          Value = riakc_obj:get_value(O),
          dict:store(K, bert:decode(Value), Acc)
      end, dict:new(), MapKeys),
    {ok, UserKeys} = riakc_pb_socket:list_keys(RiakPid, <<"users">>),
    Users = lists:foldl(fun(K, Acc) ->
          {ok, O} = riakc_pb_socket:get(RiakPid, <<"users">>, K),
          Value = riakc_obj:get_value(O),
          %Userinfo = lists:filter(fun({E,  _}) -> lists:member(E, [<<"email">>, <<"name">>, <<"color">>]) end, jsx:decode(Value)),
          Userinfo = jsx:decode(Value),
          dict:store(K, Userinfo, Acc)
      end, dict:new(), UserKeys),
    State = dict:store(users, Users, dict:store(riak, RiakPid, dict:store(maps, Maps, dict:new()))),
    {ok, State}.

handle_call(maps, _From, State) ->
    {reply, [ Info || {_, Info} <- dict:to_list(dict:fetch(maps, State))], State};
handle_call({map, Id}, _From, State) ->
    Maps = dict:fetch(maps, State),
    Reply = case dict:is_key(Id, Maps) of
        true ->
            {ok, dict:fetch(Id, Maps)};
         false ->
            {error, not_found}
    end,
    {reply, Reply, State};

handle_call(users, _From, State) ->
    Users = [ Info || {_, Info} <- dict:to_list(dict:fetch(users, State))],
    FilteredUsers = [to_client_player(User) || User <- Users],
    {reply, FilteredUsers, State};
handle_call({user, Token}, _From, State) ->
    Users = dict:fetch(users, State),
    Reply = case dict:is_key(Token, Users) of
        true ->
            {ok, dict:fetch(Token, Users)};
         false ->
            {error, not_found}
    end,
    {reply, Reply, State};
handle_call({add_user, Id, Userinfo}, _From, State) ->
    gproc:send({p, l, ws_client}, {player, to_client_player(Userinfo)}),
    State1 = dict:store(users, dict:store(Id, Userinfo, dict:fetch(users, State)), State),
    {reply, ok, State1};
handle_call({add_planet_system, Id, PlanetSystem}, _From, State) ->
    gproc:send({p, l, ws_client}, {planet_system, PlanetSystem}),
    State1 = dict:store(maps, dict:store(Id, PlanetSystem, dict:fetch(maps, State)), State),
    {reply, ok, State1};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({capture_map, PlanetSystemId, UserId}, State) ->
    Maps = dict:fetch(maps, State),
    PlanetSystem = dict:fetch(PlanetSystemId, Maps),
    PlanetSystem1 = PlanetSystem#planet_system{user_id = UserId},
    State1 = dict:store(maps, dict:store(PlanetSystemId, PlanetSystem1, Maps), State),
    erlang:start_timer(0, self(), {store_map, PlanetSystem1}),
    {noreply, State1};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, {store_map, PlanetSystem}}, State) ->
    gproc:send({p, l, ws_client}, {planet_system, PlanetSystem}),
    Store = dict:fetch(riak, State),
    Object = riakc_obj:new(<<"maps">>, PlanetSystem#planet_system.id, bert:encode(PlanetSystem)),
    riakc_pb_socket:put(Store, Object),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Internal Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server() ->
    gproc:lookup_local_name(galaxy).

to_client_player(Userinfo) ->
  lists:filter(fun({E,  _}) -> lists:member(E, [<<"email">>, <<"name">>, <<"color">>]) end, Userinfo).
