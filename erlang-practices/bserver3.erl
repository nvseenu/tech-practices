-module(bserver3).

-export([start/2]).
-export([rpc/2]).
-export([loop/3]).
-export([swap_code/2]).


start(Name, Mod) ->
	Pid = spawn(?MODULE, loop, [Name, Mod, Mod:init()]),
	register(Name, Pid).


rpc(Name, Request) ->
    io:format("~p: Sending message: ~p~n", [self(), Request]),
	Name ! {self(), Request},
	receive
		{Name, crash, Reason} -> 
			exit(Reason);
		{Name, ok, Response} -> 
			io:format("~p: Receiving response: ~p~n", [self(), Response]),
			Response;
		Any ->
			Any	
	end.	

swap_code(Name, Mod) ->
	rpc(Name, {swap_code, Mod}).	


loop(Name, Mod, OldState)->	
    receive
    	{From, {swap_code, NewMod}} ->
    		io:format("Swap code message ~n"),
    		From ! {Name, ack},
    		loop(Name, NewMod, OldState);

		{From, Request}	->
			io:format("~p: Request: ~p~n", [Name,Request]),
			try Mod:handle(Request, OldState) of
				{Response, NewState} ->
					io:format("~p: Got a resonse from callback: ~p~n", [Name, {Response, NewState}]),
					io:format("~p: Sending response to : ~p~n", [Name, From]),
					From ! {Name, ok, Response},
					loop(Name, Mod, NewState)
			catch
				_:Why ->
					From ! {Name, crash, Why},
					loop(Name, Mod, OldState)
			end    	
	end.


