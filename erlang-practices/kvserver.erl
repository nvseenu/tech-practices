-module(kvserver).
-export([start/0, loop/0, put/3, get/2]).


start() ->
	register(kvs, spawn(?MODULE, loop, [])).


loop() ->
	receive 
		{From, {put, Key, Value}} -> 
			put(Key, Value),
			From ! { self(), ok};

		{From, {get, Key}} ->
			From ! { self(), get(Key) }	
	end,
	loop().


rpc(Pid, Message) ->
	Pid ! {self(), Message},
	receive 
		{Pid, Response} -> Response
	end.

put(Pid, Key, Value) ->
	rpc(Pid, {put, Key, Value}).


get(Pid, Key) ->
	rpc(Pid, {get, Key}).	
	
