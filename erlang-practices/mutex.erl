-module(mutex).
-compile(export_all).

start() -> 
	spawn(?MODULE, free, []).


wait(Pid) ->
	Pid ! {self(), wait},
	receive 
		{Pid, ok} -> ok 
	end.

signal(Pid) ->
	Pid ! {self(), signal},
	receive 
		{Pid, ok} -> ok 
	end.	


free() ->
    io:format("Mutex is in free state~n"),
	receive 
		{Pid, wait} -> Pid ! {self(), ok},
		busy()	
	end.

busy() ->
	io:format("Mutex is in busy state~n"),
	receive
		{Pid, signal} -> Pid ! {self(), ok},
		free()		
	end.		
