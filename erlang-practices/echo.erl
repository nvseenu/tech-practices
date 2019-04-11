-module(echo).
-compile(export_all).


start() -> spawn(?MODULE, loop, []).

echo(Pid, Message) ->
	Pid ! {self(), echo, Message},
	receive 
		{Pid, Resp} -> Resp
	end.
	
stop(Pid) -> Pid ! stop.	

loop() ->
	receive 
		{Pid, echo, Message} ->	
			Pid ! {self(), Message},
			loop();

		stop ->	ok
	end.	



