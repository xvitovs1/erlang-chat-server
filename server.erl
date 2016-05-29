-module(server).
-export([start_server/0]).

start_server() ->
  % start listening on port 8080
  case gen_tcp:listen(8080,[list, {packet, 0},{active, true}, {reuseaddr, true}]) of
    {ok, Listen} ->   io:format("Server started. Listening on port 8080.~n"),
		      ets:new(users, [set, named_table, public]),
		      server_connections_loop(Listen);
    {error, Reason} -> io:format("Could not use socket on port 8080: ~s~n",[Reason]),
			exit(Reason)
  end.

% start listening for connections, for each new connection start new process
server_connections_loop(Listen) ->
 case gen_tcp:accept(Listen) of
    {ok, Socket} ->   Pid = spawn(fun() -> connect_loop(Socket) end),  % start receiving messages in new process
		      gen_tcp:controlling_process(Socket, Pid),
		      server_connections_loop(Listen);
    {error, Reason} -> io:format("error: ~s~n",[Reason]),
		       exit(Reason)
  end.

% receives messages from Socket, waits for connect message with username
connect_loop(Socket) ->
  receive
    {tcp, _, Message} ->
      case string:chr(Message,$:) of
        0 -> gen_tcp:send(Socket, "error:Unknown message: " ++ Message),
             connect_loop(Socket);
        _ -> {Action, [_|Username]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
             case Action of
               "connect" -> connect_user(remove_new_line(Username),Socket); % connect message received
               _ -> gen_tcp:send(Socket, "error:You must connect first!\n"), % some other message received
                    connect_loop(Socket)
             end
      end;
    _ -> io:format(Socket, "error: Socket closed!\n")
  end.

% main loop for receiving messages from connected user
main_loop(Socket, Username) ->
  receive
    {tcp, _, Message} ->
      case string:chr(Message,$:) of % check if the message has correct format
        0 ->  case remove_new_line(Message) of
                "disconnect" -> disconnect_user(Socket);
                _ -> gen_tcp:send(Socket, "error:Unknown message: " ++ Message),
                     main_loop(Socket, Username)
              end;
        _ ->  {Action, [_|Content]} = lists:splitwith(fun(L) -> [L] =/= ":" end, Message), % split message with ':'
              case Action of
                "bcast" -> broadcast_message(Username,Content), % broadcast
                           main_loop(Socket, Username);
                "pm" -> send_message(Username,Content,Socket), % private message
                        main_loop(Socket, Username);
                "disconnect" -> disconnect_user(Socket) % disconnect user
              end
      end;
    {tcp_closed, _} -> disconnect_user(Socket); % user closed client
    _ -> main_loop(Socket, Username)
  end.

 % checks if username is available and connects username with socket
connect_user(Username, Socket) ->
  case ets:lookup(users,Username) of
    [] -> % username was available
      % send info to others about new user
      broadcast("info:" ++ Username ++ " joined.\n"),
      ets:insert(users, {Username, Socket}),

      % confirmation to the client, might not be shown to the user
      gen_tcp:send(Socket, "info:Username assigned.\n"),
      % this welcome message is shown to the user
      gen_tcp:send(Socket, "info:Dear " ++ Username ++ ", welcome to our chat server. Leave the 'to' field empty to send your message to everyone.\n"),

      % start receiving messages
      main_loop(Socket, Username);

    [_|_] -> % username already in use
      gen_tcp:send(Socket, "error:Username already in use!\n"),
      connect_loop(Socket)
  end.

% removes new line from the beginning and the end of String
remove_new_line(String) ->
  string:strip(String, both, $\n).

 % disconnects user
disconnect_user(Socket) ->
  [[Username]] = ets:match(users, {'$1', Socket}),
  gen_tcp:send(Socket,"info:Disconnecting.\n"),
  ets:match_delete(users, {'$1', Socket}),
  gen_tcp:close(Socket),
  broadcast("info:" ++ Username ++ " disconnected.\n"). % inform everybody

% broadcasts message
broadcast_message(From, Msg) ->
  broadcast("bcast:" ++ From ++ ": " ++ Msg).

% broadcast 'msg' string to everyone
broadcast(Msg) ->
  ets:foldr(fun({_, Socket}, _) ->
                gen_tcp:send(Socket, Msg)
            end, notused,users).

% sends private message
send_message(From,Content,Socket) ->
  {To,[_|Message]} = lists:splitwith(fun(L) -> [L] =/= ":" end, Content), % get receiver
  case ets:lookup(users,To) of
    [] -> gen_tcp:send(Socket, "error:User " ++ To ++ " does not exist!\n");
    [{_,USocket}|_] -> gen_tcp:send(USocket,"pm:" ++ From ++ ": " ++ Message)
  end.
