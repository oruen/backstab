-module(backstab_ws_handler).
-include("backstab.hrl").
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Pid} = supervisor:start_child(backstab_battle_sup, [{<<"map-id">>, <<"1">>, self()}]),
    {ok, Req, Pid}.

websocket_handle({binary, <<_Ws, Msg/binary>>}, Req, Pid) ->
    DecodedMsg = bert:decode(Msg),
    gen_server:cast(Pid, DecodedMsg),
    %handle_request(DecodedMsg, State),
    {ok, Req, Pid};
    %{reply, {binary, bert:encode(DecodedMsg)}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, {send, Type, Data}}, Req, State) ->
    Msg = bert:encode({Type, Data}),
    {reply, {binary, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
