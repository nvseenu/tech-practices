-module(event_server).
-compile(export_all).
-include("event.hrl").

-record(subscriber, {
	pid,
	event_entries=#{}
	}).
-record(event_entry, {
	event,
    worker_id,
    worker_ref
	}).




start() ->
	spawn(fun() -> 
		process_flag(trap_exit, true),
		loop(new_subscribers())
	end).

subscribe(Pid, Server) ->
	Server! {Pid, {subscribe, Pid}},
	receive 
		{Server, Resp} -> Resp
	end.

add_event(Event = #event{}, Server) ->
	Server! {self(), {add, Event}},
	receive 
		{Server, Resp} -> Resp
	end.

cancel_event(Name, Server) ->
	Server! {self(), {cancel, Name}},
	receive 
		{Server, Resp} -> Resp
	end.

stop(Server) ->
	Server! {self(), {stop}},
	receive 
		{Server, Resp} -> Resp
	end.	


loop(Subscribers) ->
    io:format("Current State: ~p~n", [Subscribers]),
    Server = self(),
	receive 	
		{_ProcessId, {done, Name, SubscriberPid}} -> 
			%% find subscriber who owns the event 
			SubscriberPid ! {Server, {done, Name}},
			loop(Subscribers);		

		{From, {shutdown}} ->
			From ! {Server, ok};

		{From, {stop}}	->
			From ! {Server, ok};			

		{From, Message} ->		    		    
		    Subscribers1 = handle_message({From, Message}, Subscribers),
			loop(Subscribers1);				
			
        Any ->
			io:format("Unexpected message: ~p!n", [Any]),
			loop(Subscribers)				
	end.


%% message handling functions  %%
handle_message({From, {subscribe, Pid}}, Subscribers) ->
	Subscribers1 = add_subscriber(Pid, Subscribers),		    	
	From ! {self(), ok},
	Subscribers1;

handle_message({From, {add, Event=#event{}}}, Subscribers) ->
	{ok, Subscriber} = find_subscriber(From, Subscribers),	
	{WorkerId, WorkerRef} = event_worker:start_monitor(Event, Subscriber#subscriber.pid, self()),
	Subscribers1 = add_event_entry(Event, WorkerId, WorkerRef, From, Subscribers),
	From ! {self(), ok},
	Subscribers1;

handle_message({From, {cancel, Name}}, Subscribers) ->
    io:format("cacnelling event:~p ~n", [Name]),
	{ok, Subscriber} = find_subscriber(From, Subscribers),
	{ok, EventEntry} = find_event_entry(Name, Subscriber),
	io:format("Matching event entry: ~p ~n", [EventEntry]),
	ok = event_worker:cancel({EventEntry#event_entry.worker_id, EventEntry#event_entry.worker_ref}),
    From ! {self(), ok},
	Subscribers.


%%  functions to manipulate event server internal state %%%

new_subscribers() -> #{}.

add_subscriber(Pid, Subscribers) -> 	
	Subscriber = #subscriber{pid=Pid},
	maps:put(Subscriber#subscriber.pid, Subscriber, Subscribers).

find_subscriber(Pid, Subscribers) ->
	maps:find(Pid, Subscribers).

update_subscriber(Subscriber, Subscribers) -> 
	maps:put(Subscriber#subscriber.pid, Subscriber, Subscribers).	

add_event_entry(Event = #event{}, WorkerId, WorkerRef, SubscriberId, Subscribers) ->				
 	{ok, Subscriber} = find_subscriber(SubscriberId, Subscribers),
 	Entry = #event_entry{event=Event, worker_id=WorkerId, worker_ref=WorkerRef},
	Entries = maps:put(Event#event.name, Entry, Subscriber#subscriber.event_entries),
	Subscriber1 = #subscriber{pid=SubscriberId, event_entries=Entries},
	update_subscriber(Subscriber1, Subscribers).

find_event_entry(Name, Subscriber) ->
	maps:find(Name, Subscriber#subscriber.event_entries).			
