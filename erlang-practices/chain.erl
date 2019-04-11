-module(chain).
-compile(export_all).


chain(0) ->
	io:format("Chain0 with pid: ~p starts~n", [self()]),
	timer:sleep(2000),
	io:format("Chain0 is diying");
	%erlang:error("Died");


chain(N) ->
	io:format("Chain~p with pid: ~p starts~n", [N, self()]),
	spawn_link(fun() -> chain(N-1) end),
	io:format("Chain~p with pid: ~p waiting for messages~n", [N, self()]),
	receive 
		X -> io:format("Chain~p got message: ~p~n", [N, X])
	end.	




process_a(TrapExit, F) ->
    io:format("ProcessA starts process B~n"),
    case TrapExit of
    	true -> process_flag(trap_exit, true);
    	false -> ok
    end,	
	spawn_link(fun() -> process_b(F) end),
	io:format("ProcessA waiting ~n"),
	loop().

loop() ->
  	io:format("ProcessA waiting ~n"),
	receive 
		X -> io:format("ProcessA got message: ~p~n", [X])
	end,
	loop().


process_b(F) ->	
    io:format("ProcessB starts ~n"),
	timer:sleep(2000),
	    io:format("ProcessB ends ~n"),
	F().

