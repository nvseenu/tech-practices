-module(event_handler).
-export([make/1]).
-export([add_handler/2]).
-export([event/2]).
-export([loop/1]).


make(Name) ->
	register(Name, spawn(fun() -> 
		loop(fun  no_op/1)
	end)). 

add_handler(Name , Fun) ->
	Name ! {add, Fun}.

event(Name, X) -> Name ! {event, X}.


loop(Fun)	->
	receive
		{add, Fun1} ->
			loop(Fun1);
		{event, Any} ->
			(catch Fun(Any)),
            loop(Fun)
	end. 

no_op(_) -> void.	