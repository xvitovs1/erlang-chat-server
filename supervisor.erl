-module(supervisor).
-export([start/0]).

start() ->
  Pid = spawn(server, start_server, []),
  on_exit(Pid).

on_exit(Pid) ->
  spawn(fun() ->
            process_flag(trap_exit, true),
            link(Pid),
            receive
              {'EXIT', Pid, normal} ->
                io:format("Server exited normally.~n");
              {'EXIT', Pid, Why} ->
                io:format("Server exited with error. Will restart in 10 seconds.~n"),
                timer:apply_after(10000,erlang, spawn,[server, start_server, []])
            end
          end).
