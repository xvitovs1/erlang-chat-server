-module(server).
-export([start_server/0).
-behavior(gen_server).

start_server() ->
  {ok, Listen} = gen_tcp:listen(8080,[list, {packet, 0}, {reuseaddr, true}]),
  io:format("Server started. Listening on port 8080."),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

loop(Socket) ->
  receive
    {connect, Username} -> connect_user(Username, Socket);
    {disconnect} -> disconnect_user()
  end.

connect_user(Username, Socket) ->





