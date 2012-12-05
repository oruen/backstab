-module(backstab_maps).
-export([load/1]).
-include("backstab.hrl").

load(_MapId) ->
    [{planets, [
        #planet{id = <<"1">>, capacity = 30,
                type = <<"ground">>, quantity = 10, user_id = <<"1">>},
        #planet{id = <<"2">>, capacity = 40, type = <<"ground">>},
        #planet{id = <<"3">>, capacity = 50,
                type = <<"ground">>, quantity = 15, user_id = <<"2">>},
        #planet{id = <<"4">>, capacity = 50, type = <<"ground">>}]},
     {routes, [
         #route{from = <<"1">>, to = <<"2">>},
         #route{from = <<"1">>, to = <<"3">>},
         #route{from = <<"2">>, to = <<"3">>},
         #route{from = <<"3">>, to = <<"4">>}]}
   ].
