%%
%% backstab.erl
%% backstab entry point
%%
-module(backstab).
-include("backstab.hrl").

-export([start/0, start_link/0, stop/0]).

start_link() ->
    backstab_sup:start_link().

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(gproc),
    ok = application:start(backstab).

stop() ->
    application:stop(backstab).

