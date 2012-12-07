-module(create_planet_handler).

-include("backstab.hrl").

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
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
    case riakc_pb_socket:get(RiakPid, <<"users">>, Token) of
        {ok, _} ->
            backstab_maps:store(backstab_maps:random(), Token, RiakPid),
            {ok, Req2} = cowboy_req:reply(201, [], <<"created">>, Req);
        {error, notfound} ->
            {ok, Req2} = cowboy_req:reply(404, [], <<"not found">>, Req)
    end,
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

