-module(server).
-export([start_server/0]).

start_server() ->
  {ok, Listen} = gen_tcp:listen(8080,[list, {packet, 0},{active, true}, {reuseaddr, true}]),
  io:format("Server started. Listening on port 8080.~n"),
  ets:new(users, [set, named_table, public]),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

loop(Socket) ->
  receive
        {tcp, _, Message} ->
            {Action, [_|Username]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case Action of
                "connect" -> connect_user(remove_new_line(Username),Socket);
                _ -> gen_tcp:send(Socket, "You must connect first!\n"),
                     loop(Socket)
            end;
        _ -> gen_tcp:send(Socket, "Error!\n"),
             ok
  end.

main_loop(Socket, Username) ->
   receive
        {tcp, _, Message} ->
            {Action, [_|Content]} = lists:splitwith(fun(L) -> [L] =/= ":" end, Message),
            case Action of
                "bcast" -> broadcast_message(Content),
                           main_loop(Socket, Username);
                "send" -> send_message(Username,Content,Socket);
                "disconnect" -> disconnect_user(Socket)
            end;
        _ -> main_loop(Socket, Username)
    end.

connect_user(Username, Socket) ->
    case ets:lookup(users,Username) of
      [] -> broadcast_message(Username ++ " joined.\n"),
              ets:insert(users, {Username, Socket}),
              main_loop(Socket, Username);
      [_|_] -> gen_tcp:send(Socket, "Username already in use!\n"),
            loop(Socket)
    end.

remove_new_line(Username) ->
  string:strip(Username, both, $\n).

disconnect_user(Socket) ->
  gen_tcp:send(Socket,"Disconnecting.\n"),
  ets:match_delete(users, Socket).

broadcast_message(Msg) ->
    ets:foldr(fun({_, Socket}, _) ->
        gen_tcp:send(Socket, Msg)
    end, notused,users).

send_message(From,Content,Socket) ->
  {To,[_|Message]} = lists:splitwith(fun(L) -> [L] =/= ":" end, Content),
  case ets:lookup(users,To) of
    [] -> gen_tcp:send(Socket,"User " ++ To ++ " does not exist!\n");
    [{_,USocket}|_] -> gen_tcp:send(USocket,From++ ": " ++ Message)
  end.
