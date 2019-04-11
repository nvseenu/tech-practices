-module(monitor1).
-export([start_process/0, monitor_process/1]).

start_process() ->
	spawn(fun () ->
		io:format("Waiting for message~n"),
		receive 
			X -> list_to_atom(X)			
		end
	end).

monitor_process(Pid) ->
	spawn(fun() ->
		Ref = monitor(process, Pid),
		io:format("Ref ~p~n", [Ref]),
		io:format("Waiting for down message~n"),
		receive 
			{'DOWN', Ref, process, Pid, Why} ->
				io:format("Process ~p is down with reason: ~p and Ref: ~p~n", [Pid, Why, Ref]);
            Any -> 
                io:format("Any: ~p~n", [Any])             
		end
	end).
	



	