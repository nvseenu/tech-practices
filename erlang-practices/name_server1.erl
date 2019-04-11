-module(name_server1).
-export([add/2]).
-export([find/1]).
-export([handle/2]).
-export([init/0]).

-import(bserver3, [rpc/2]).

add(Name, Place) ->
	rpc(name_server, {add, Name, Place}).

find(Name) ->
	rpc(name_server, {find, Name}).	

init() ->	
	maps:new().

handle({add, Name, Place}, State) ->	
	{ok, maps:put(Name, [Place], State)};

handle({find, Name}, State) ->	
	{maps:find(Name, State), State}.	

	

