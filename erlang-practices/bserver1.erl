-module(bserver1).
-export([start/2, loop/3, rpc/2]).

start(Name, Mod) ->
	Pid = spawn(?MODULE, loop, [Name, Mod, Mod:init()]),
	register(Name, Pid).


loop(Name, Mod, State) ->
	receive
		{From, Request} ->
			{Response, State1} = Mod:handle(Request, State),
			From ! {Name, Response},
			loop(Name, Mod, State1)
	end.


rpc(Name, Request) ->
	Name ! { self(), Request },
	receive 
		{Name, Response} -> Response			
	end.
