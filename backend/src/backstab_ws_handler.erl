-module(backstab_ws_handler).
-include("backstab.hrl").
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    erlang:start_timer(0, self(), {global, init}),
    {ok, Req, [{riak, Pid}]}.
    %{ok, Pid} = supervisor:start_child(backstab_battle_sup, [{<<"map-id">>, <<"1">>, self()}]),
    %{ok, Req, Pid}.

websocket_handle({binary, <<_Ws, Msg/binary>>}, Req, Pid) ->
    DecodedMsg = bert:decode(Msg),
    gen_server:cast(Pid, DecodedMsg),
    %handle_request(DecodedMsg, State),
    {ok, Req, Pid};
    %{reply, {binary, bert:encode(DecodedMsg)}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, {global, init}}, Req, [{riak, RiakPid}]) ->
    {ok, MapKeys} = riakc_pb_socket:list_keys(RiakPid, <<"maps">>),
    Maps = lists:map(fun(K) ->
          {ok, O} = riakc_pb_socket:get(RiakPid, <<"maps">>, K),
          Value = riakc_obj:get_value(O),
          PlanetSystem = bert:decode(Value),
          Map = PlanetSystem#planet_system.map,
          Map
      end, MapKeys),
    erlang:start_timer(0, self(), {send, global_map, Maps}),
    {ok, Req, [riak, RiakPid]};

websocket_info({timeout, _Ref, {send, Type, Data}}, Req, State) ->
    Msg = bert:encode({Type, Data}),
    {reply, {binary, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
