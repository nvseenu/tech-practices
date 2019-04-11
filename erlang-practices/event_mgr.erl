-module(event_mgr).
-compile(export_all).

start(Name, HandlerList) ->	
	Pid = spawn(?MODULE, loop, [#{}]),	
	register(Name, Pid).


add_handler(Name, Handler, Data) -> ok.

delete_handler(Name, Handler) -> ok.

send_event(Name, Event) -> ok.

stop() -> ok.


loop(Handlers) -> 
	receive
		{add_handler, From, Name, HandlerModule, Data} ->
			NewHandlers = case maps:find(Name, Handlers) of
				{ok, _} ->
					 From ! {self(), error, already_exist},
					 Handlers;

				error -> 
				    HandlerState = HandlerModule:init(Data),
					Handlers1 = maps:put(Name, {HandlerModule, HandlerState}, Handlers),
					From ! {self(), ok},
					Handlers1
			end,
            loop(NewHandlers);

		{delete_handler, From, Name, HandlerModule}	->
			NewHandlers = case maps:find(Name, Handlers) of
				{ok, {HandlerMod, HandlerState}} -> 
					ok = HandlerMod:terminate(HandlerState),
					Handlers1 = maps:remove(Name, Handlers),
					From ! {self(), ok},
					Handlers1;

				error -> 
					From ! {self(), error, not_found},
					Handlers	
			end, 
			loop(NewHandlers);

		{event, From, Name, Event} -> 
			case maps:find(Name, Handlers) of
				{ok, {HandlerMod, HandlerState}} -> 
					ok = HandlerMod:event(Event, HandlerState),
					From ! {self(), ok};

				error -> 
					From ! {self(), error, not_found}
						
			end, 
			loop(Handlers);
			

		{stop, From} -> From ! {self(), ok}
	end.

