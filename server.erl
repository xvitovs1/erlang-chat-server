-module(server).
-export([start_server/0).

start_server() ->
  {ok, Listen} = gen_tcp:listen(8080,[list, {packet, 0}, {reuseaddr, true}]),
  io:format("Server started. Listening on port 8080."),
  UsersTabId = ets:new(users, [bag]),
  spawn(fun() -> par_connect(Listen, UsersTabId) end).

par_connect(Listen, UsersTabId) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen, UsersTabId) end),
  loop(Socket, UsersTabId).

loop(Socket) ->
  receive
    {connect, Username} -> connect_user(UsersTabId, Username, Socket);
    {bcast, Username, Msg} -> broadcast_message(Username, Msg);
    {disconnect} -> disconnect_user()
  end.

connect_user(UsersTabId, Username, Socket) ->
    broadcast_message(UsersTabId, Username, Username + " joined.").
    ets:insert(UsersTabId, {Username, Socket}).

broadcast_message(UsersTabId, Msg) ->
    ets:foldr(fun({Nick, Socket}, _) ->
        % send
    end, notused, UsersTabId).

