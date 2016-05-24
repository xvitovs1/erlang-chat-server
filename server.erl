-module(server).
-export([loop/0, start/0).


start_parallel_server() ->
  {ok, Listen} = gen_tcp:listen(...),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, _, Bin } ->


start() ->
   io:format("Server started."),
   loop().

