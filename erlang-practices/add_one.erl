-module(add_one).
-compile(export_all).

start() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(add_one, loop, []),
	register(add_one, Pid).

request(Num) ->
	add_one ! {request, self(), Num},
	receive 
		{result, Resp} -> Resp;
		{'EXIT', _Pid, Reason} -> {error, Reason}
    	after 1000 -> timeout
    end.		

loop() ->
	receive 
		{request, From, Num} ->
			From ! {result, Num + 1},
			loop();
		stop -> ok
	end.	



