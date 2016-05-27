-module(server).
-export([start_server/0]).

start_server() ->
  case gen_tcp:listen(8080,[list, {packet, 0},{active, true}, {reuseaddr, true}]) of
    {ok, Listen} ->   io:format("Server started. Listening on port 8080.~n"),
			    ets:new(users, [set, named_table, public]),
			    spawn(fun() -> par_connect(Listen) end);
    {error, Reason} -> io:format("Could not use socket on port 8080: ~s~n",[Reason]),
			exit(1)
  end.

par_connect(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->   {ok, Socket} = gen_tcp:accept(Listen),
		      spawn(fun() -> par_connect(Listen) end),
		      loop(Socket);
    {error, Reason} -> io:format("error: ~s~n",[Reason])
  end.

loop(Socket) ->
  receive
        {tcp, _, Message} ->
	    case string:chr(Message,$:) of
	      0 ->  gen_tcp:send(Socket, "error:Unknown message: " ++ Message),
		    loop(Socket);
	      _ ->  {Action, [_|Username]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
		    case Action of
			"connect" -> connect_user(remove_new_line(Username),Socket);
			_ -> gen_tcp:send(Socket, "error:You must connect first!\n"),
			    loop(Socket)
		    end
	    end;
        _ -> gen_tcp:send(Socket, "error:Error!\n"),
             ok
  end.

main_loop(Socket, Username) ->
   receive
	 {tcp, _, Message} ->
	    case string:chr(Message,$:) of
	      0 ->  case remove_new_line(Message) of
		      "disconnect" -> disconnect_user(Socket);
		      _ -> gen_tcp:send(Socket, "error:Unknown message: " ++ Message),
			   main_loop(Socket, Username)
		    end;
	      _ ->  {Action, [_|Content]} = lists:splitwith(fun(L) -> [L] =/= ":" end, Message),
		    case Action of
			"bcast" -> broadcast_message(remove_new_line(Content)),
				   main_loop(Socket, Username);
			"send" -> send_message(Username,remove_new_line(Content),Socket),
				  main_loop(Socket, Username);
			"disconnect" -> disconnect_user(Socket)
		    end
	    end;
        _ -> main_loop(Socket, Username)
    end.

connect_user(Username, Socket) ->
    case ets:lookup(users,Username) of
      [] -> broadcast_message(Username ++ " joined.\n"),
          ets:insert(users, {Username, Socket}),
          gen_tcp:send(Socket, "info:Username assigned.\n"),
          main_loop(Socket, Username);
      [_|_] -> gen_tcp:send(Socket, "error:Username already in use!\n"),
            loop(Socket)
    end.

remove_new_line(String) ->
  string:strip(String, both, $\n).

disconnect_user(Socket) ->
  gen_tcp:send(Socket,"info:Disconnecting.\n"),
  ets:match_delete(users, Socket).

broadcast_message(Msg) ->
    ets:foldr(fun({_, Socket}, _) ->
        gen_tcp:send(Socket, Msg)
    end, notused,users).

send_message(From,Content,Socket) ->
  {To,[_|Message]} = lists:splitwith(fun(L) -> [L] =/= ":" end, Content),
  case ets:lookup(users,To) of
    [] -> gen_tcp:send(Socket, "bcast:User " ++ To ++ " does not exist!\n");
    [{_,USocket}|_] -> gen_tcp:send(USocket,From ++ ": " ++ Message)
  end.
