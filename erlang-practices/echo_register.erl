-module(echo_register).
-compile(export_all).

start() ->
	Pid = spawn(?MODULE, loop, []),
	register(echo, Pid).

echo(Message) ->
	echo ! {self(), Message},
	receive
		{_Pid, Resp} ->	Resp
	end.

stop() -> echo ! stop.

loop() ->
	receive 
		{From, Message}	-> 
			From ! {self(), Message},
			loop();
		stop -> ok
	end.		 	 