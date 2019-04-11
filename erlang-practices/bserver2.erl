-module(bserver2).
-export([start/2]).
-export([loop/3]).
-export([rpc/2]).


start(Name, Mod) ->
	Pid = spawn(?MODULE, loop, [Name, Mod, Mod:init()]),
	register(Name, Pid).

loop(Name, Mod, OldState) -> 
	receive 
		{From, Request} ->
			try Mod:handle(Request, OldState) of
				{Response, NewState} -> 
					From ! {Name, {ok, Response}},
					loop(Name, Mod, NewState)
			catch
				_:Why ->
					io:format("Error occurred with ~p~n", [Why]),
					From ! { Name, {crash, Why}},
					loop(Name, Mod, OldState)
			end		
	end.


rpc(Name, Request) ->
	Name ! {self(), Request},
	receive 
		{Name, {crash, Reason}} -> exit(Reason);
		{Name, {ok, Response}} -> Response		
	end.


