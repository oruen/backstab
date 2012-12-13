-module(backstab_ws_handler).
-include("backstab.hrl").
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {Token, _} = cowboy_req:qs_val(<<"token">>, Req),
    {ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    case riakc_pb_socket:get(RiakPid, <<"users">>, Token) of
        {ok, O} ->
            Userinfo = jsx:decode(riakc_obj:get_value(O)),
            {_, Email} = lists:keyfind(<<"email">>, 1, Userinfo),
            gproc:reg({p, l, Email}),
            erlang:start_timer(0, self(), {global, init}),
            State = dict:store(battle, false, dict:store(player, Userinfo, dict:store(riak, RiakPid, dict:new()))),
            {ok, Req, State};
        {error, notfound} ->
            {stop, Req, []}
    end.
    %{ok, Pid} = supervisor:start_child(backstab_battle_sup, [{<<"map-id">>, <<"1">>, self()}]),
    %{ok, Req, Pid}.

websocket_handle({binary, <<_Ws, Msg/binary>>}, Req, State) ->
    DecodedMsg = bert:decode(Msg),
    message_handle(DecodedMsg, Req, State);

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

message_handle({global, fight, MapId}, Req, State) ->
    Userinfo= dict:fetch(player, State),
    {_, Email} = lists:keyfind(<<"email">>, 1, Userinfo),
    {ok, Pid} = supervisor:start_child(backstab_battle_sup, [{MapId, Email, self()}]),
    {ok, Req, dict:store(battle, Pid, State)};

message_handle({global, defend, BattleId}, Req, State) ->
    Userinfo = dict:fetch(player, State),
    {_, Email} = lists:keyfind(<<"email">>, 1, Userinfo),
    Battle = list_to_pid(binary_to_list(BattleId)),
    case is_process_alive(Battle) of
        true ->
            gen_server:call(Battle, {enter_battle, Email}),
            {ok, Req, dict:store(battle, Battle, State)};
        false ->
            {ok, Req, State}
    end;

message_handle(Msg, Req, State) ->
    Battle = dict:fetch(battle, State),
    Userinfo = dict:fetch(player, State),
    gen_server:cast(Battle, {Msg, Userinfo}),
    {ok, Req, State}.

websocket_info({timeout, _Ref, {global, init}}, Req, State) ->
    RiakPid = dict:fetch(riak, State),
    {ok, UserKeys} = riakc_pb_socket:list_keys(RiakPid, <<"users">>),
    Users = lists:map(fun(K) ->
          {ok, O} = riakc_pb_socket:get(RiakPid, <<"users">>, K),
          Value = riakc_obj:get_value(O),
          lists:filter(fun({E,  _}) -> lists:member(E, [<<"email">>, <<"name">>, <<"color">>]) end, jsx:decode(Value))
      end, UserKeys),
    {ok, MapKeys} = riakc_pb_socket:list_keys(RiakPid, <<"maps">>),
    Maps = lists:map(fun(K) ->
          {ok, O} = riakc_pb_socket:get(RiakPid, <<"maps">>, K),
          Value = riakc_obj:get_value(O),
          bert:decode(Value)
      end, MapKeys),
    erlang:start_timer(0, self(), {send, global_map, [Users, Maps]}),
    {ok, Req, State};

websocket_info({timeout, _Ref, {send, Type, Data}}, Req, State) ->
    Msg = bert:encode({Type, Data}),
    {reply, {binary, Msg}, Req, State};
websocket_info(Info, Req, State) ->
    Userinfo = dict:fetch(player, State),
    {_, Email} = lists:keyfind(<<"email">>, 1, Userinfo),
    ?PRINT(Info),
    case Info of
        {defend, Email, Address} ->
            {reply, {binary, bert:encode({assault, Address})}, Req, State};
        _ ->
            {ok, Req, State}
    end.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
