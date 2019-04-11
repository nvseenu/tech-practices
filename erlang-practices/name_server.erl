-module(name_server).
-export([init/0, add/2, find/1, handle/2]).
-import(bserver3, [rpc/2]).

add(Name, Place) -> rpc(name_server, {add, Name, Place}).
find(Name) -> rpc(name_server, {find, Name}).


%% call back functions
init() -> 
	io:format("iNIT callback called ~n"),
	#{}.

handle({add, Name, Place}, State) ->
	io:format("Add callback called ~n"),
	State1 = maps:put(Name, Place, State),
	{ok, State1};

handle({find, Name}, State)	->
	io:format("Find callback called ~n"),
	{maps:get(Name, State), State}.

