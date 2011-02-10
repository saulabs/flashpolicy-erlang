-module(server).

-export([start/0]).

-ifdef(TEST).
%-include("test/server_test.erl").
-endif.


start() ->
  io:format("starting server ~n").
  