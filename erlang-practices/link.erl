-module(link).
-compile(export_all).

start(Pid) ->
	spawn(fun() -> 
		if 
			is_pid(Pid) ->  
				link(Pid),
				io:format("Pid:~p linked with given pid:~p ~n",[self(), Pid]),
				process_flag(trap_exit, true),
				loop();
			true -> 
				io:format("No pid to  link~n"),
				loop()
		end				
	end).

loop() ->
	receive 
		{'Exit', Pid, Reason} ->
		 	
		Any -> 
			io:format("~p: Received message: ~p~n", [self(), Any]),
			loop()
		}
	end.

