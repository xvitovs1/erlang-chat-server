-module(client).
-export([connect/0]).
-import(server, [start_server/0]).

connect() ->
  server:start_server(),
  {ok,Socket} = gen_tcp:connect("localhost",8080,[list,{packet,0}]),
  gen_tcp:send(Socket,"connect:NewUser").
