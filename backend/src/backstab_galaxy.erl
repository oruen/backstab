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

-export([maps/0, users/0, user/1, map/1]).

%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

maps() ->
    gen_server:call(server(), maps).

map(Id) ->
    gen_server:call(server(), {map, Id}).

users() ->
    gen_server:call(server(), users).

user(Token) ->
    gen_server:call(server(), {user, Token}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    FilteredUsers = [lists:filter(fun({E,  _}) -> lists:member(E, [<<"email">>, <<"name">>, <<"color">>]) end, User) || User <- Users],
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
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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
