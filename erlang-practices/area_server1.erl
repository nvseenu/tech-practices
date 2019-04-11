-module(area_server1).
-export([loop/0, rpc/2]).
-record(subscriber, {
	pid,
	event_entries=#{}
	}).
-record(event_entry, {
	event,
    worker_id,
    worker_ref
	}).




rpc(Pid, Message) ->
	Pid ! {self(), Message},
	receive
		Response -> Response
	end.

loop() ->
	receive 
		{From, {rectangle, Width, Height}} ->
			From ! Width*Height;
			
		{From, {square, Side}} ->
			From ! Side * Side;
			
		{From, Other} ->
			From ! {error, Other}

	end,
	loop().
