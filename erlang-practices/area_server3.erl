-module(area_server3).
-export([start/0, square/2, rect/3, loop/0]).

start() ->
	spawn(area_server3, loop, []).


square(Pid, Side) ->
	Pid ! {self(), {square, Side}},
	receive 
		{Pid, Resp} -> Resp;
		Default -> Default	
	end.	


rect(Pid, Height, Width) ->
	Pid ! {self(), {rectangle, Width, Height}},
	receive 
		{Pid, Resp} -> Resp	
	end.	


loop() ->
	receive 
		{From, {rectangle, Width, Height}} ->
				From ! {self(), Width * Height};
		{From, {square, Side}} ->
				From ! {self(), Side * Side};
		{From, Other} ->
				From ! {self(), {error, Other}}
	end,
	loop().			

