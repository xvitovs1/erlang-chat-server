-module(server).
-export([start_server/0]).

start_server() ->
  {ok, Listen} = gen_tcp:listen(8080,[list, {packet, 0},{active, true}, {reuseaddr, true}]),
  io:format("Server started. Listening on port 8080.~n"),
  UsersTabId = ets:new(users, [bag]),
  spawn(fun() -> par_connect(Listen, UsersTabId) end).

par_connect(Listen, UsersTabId) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen, UsersTabId) end),
  loop(Socket, UsersTabId).

loop(Socket, UsersTabId) ->
  receive
        {ok, Message} ->
            io:format("Message: ~p~n", [Message]),
            {Action, [Username|_]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            io:format("Username: ~p~n", [Username]),
            case Action of
                "connect" -> connect_user(UsersTabId, Username,Socket);
                _ ->
                    gen_tcp:send(Socket, "You must connect first!\n"),
                    io:format("Some problem.~n"),
                    ok
            end;
        {error, closed} ->
            io:format("error~n"),
            ok
  end.

main_loop(Socket, UsersTabId, Username) ->
   receive
        {ok, Message} ->
            io:format("Message: ~p~n", [Message]),
            {Action, [Content|_]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case Action of
                "bcast" -> broadcast_message(UsersTabId, Content),
                           main_loop(Socket, UsersTabId, Username);
                "disconnect" -> disconnect_user(UsersTabId, Socket)
            end;
        {error, closed} ->
            ok
    end.

connect_user(UsersTabId, Username, Socket) ->
    broadcast_message(UsersTabId, Username ++ " joined."),
    ets:insert(UsersTabId, {Username, Socket}),
    main_loop(Socket, UsersTabId, Username).

disconnect_user(UsersTabId, Socket) ->
  ets:match_delete(UsersTabId, Socket).

broadcast_message(UsersTabId, Msg) ->
    io:format("Broadcasting~n"),
    ets:foldr(fun({Nick, Socket}, _) ->
        gen_tcp:send(Socket, Msg)
    end, notused, UsersTabId).

send_message(From,Socket, Msg) ->
  gen_tcp:send(Socket,From++ ": " ++ Msg).
