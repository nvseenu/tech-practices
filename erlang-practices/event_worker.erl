-module(event_worker).
-compile(export_all).
-include("event.hrl").

start_monitor(Event = #event{}, SubscriberPid, Server) ->
	spawn_monitor(?MODULE, loop, [{Event, SubscriberPid, Server}]).


cancel({WorkerId, _WorkerRef}) ->
	WorkerId ! {self(), cancel},
	receive 
		{WorkerId, ok} -> ok
	end.

loop({Event, SubscriberPid, Server}) ->
	io:format("Worker:~p is running with Event:~p~n", [self(), Event]),
	receive 
		{From, cancel} -> 				
			From ! {self(), ok};
		Any ->
			io:format("unexected msg: ~p~n", Any),
			loop({Event, SubscriberPid, Server})
        after Event#event.timeout ->
        	io:format(">>>>>>>>>>>>>>Worker:~p got timed out:~p~n", [self(), Event]),
        	Server ! {self(), {done, Event#event.name, SubscriberPid}}
	end.


