%%
%% backstab_app.erl
%% backstab application
%%
-module(backstab_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ~~~~~~~~~~~~~~~~~~~~~
%% Application callbacks
%% ~~~~~~~~~~~~~~~~~~~~~

start(_StartType, _StartArgs) ->
    Dispatch = [
      {'_', [
        {[<<"ws">>], backstab_ws_handler, []},
        {['...'], cowboy_static, [
          {directory, {priv_dir, backstab, []}},
          {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
        ]}
      ]}
    ],
    {ok, _} = cowboy:start_http(http, 1000, [{port, 8080}], [
      {dispatch, Dispatch}
    ]),

    backstab_sup:start_link().

stop(_State) ->
    ok.

