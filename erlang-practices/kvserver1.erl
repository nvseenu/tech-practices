-module(kvserver1).
-export([start/0, loop/0, put_key/2, get_key/1]).


start() ->
	register(kvs, spawn(?MODULE, loop, [])).


loop() ->
	receive 
		{From, {put, Key, Value}} -> 
			put(Key, {ok, Value}),
			From ! { kvs, ok};

		{From, {get, Key}} ->
			From ! { kvs, get(Key) }	
	end,
	loop().


rpc(Message) ->
	kvs ! {self(), Message},
	receive 
		{kvs, Response} -> Response
	end.

put_key(Key, Value) ->
	rpc({put, Key, Value}).


get_key(Key) ->
	rpc({get, Key}).	
	
