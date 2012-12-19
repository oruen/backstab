-module(create_planet_handler).

-include("backstab.hrl").

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    random:seed(erlang:now()),
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            create(Req, State);
        _ ->
          {ok, Req2} = cowboy_req:reply(404, [], <<"not supported">>, Req),
          {ok, Req2, State}
    end.

create(Req, State) ->
    {Token, _} = cowboy_req:qs_val(<<"token">>, Req),
    {ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, Req2} = case riakc_pb_socket:get(RiakPid, <<"users">>, Token) of
        {ok, O} ->
            Player = jsx:decode(riakc_obj:get_value(O)),
            ok = backstab_galaxy:add_user(Token, Player),
            {_, Email} = lists:keyfind(<<"email">>, 1, Player),
            {ok, PlanetSystem} = backstab_maps:store(backstab_maps:random(), Email, RiakPid),
            ok = backstab_galaxy:add_planet_system(PlanetSystem#planet_system.id, PlanetSystem),
            cowboy_req:reply(201, [], <<"created">>, Req);
        {error, notfound} ->
            cowboy_req:reply(404, [], <<"not found">>, Req)
    end,
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

