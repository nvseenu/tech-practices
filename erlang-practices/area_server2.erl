-module(area_server2).
-export([rpc/2, loop/0]).

rpc(Pid, Message) ->
	Pid ! {self(), Message},
	receive
		{Pid, Resp} -> Resp
	end.


loop() ->
	receive
		{From, {rectangle, Width, Height}}	->
			From ! {self(), Width*Height};
		{From, {square, Side}}	->
			From ! {self(), Side*Side};
		{From, Other} ->
			From ! {self(), Other}
	end,
	loop().
