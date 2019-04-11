-module(kitchen).
-compile(export_all).

start() ->
	spawn(?MODULE, fridge1, [[]]).

store(Pid, Food) ->
	Pid ! { self(), {store, Food}},
	receive 
		{Pid, Resp} -> Resp
	end.
		
take(Pid, Food) ->
	Pid ! { self(), {take, Food}},
	receive 
		{Pid, Resp} -> Resp
	end.		



store1(Pid, Food) ->
	Pid ! { self(), {store, Food}},
	receive 
		{Pid, Resp} -> Resp
		after 1000 -> timeout			
	end.
		
take1(Pid, Food) ->
	Pid ! { self(), {take, Food}},
	receive 
		{Pid, Resp} -> Resp
		after 1000 -> timeout			
	end.		



fridge1(Foods) ->
		receive 
			{From, {store, Food}} ->
					io:format("Processign get requesst~n"),
					From ! {self(), ok},
					fridge1([Food|Foods]);
			{From, {take, Food}} ->
					io:format("Processign take requesst~n"),
					case lists:member(Food, Foods) of
							true -> 
									From ! {self(), {ok, Food}},
									fridge1(lists:delete(Food, Foods));
							false ->
									From ! {self(), not_found},
									fridge1(Foods)
					end;
			{_From, terminate} -> ok		
		end.


restarter() ->
    io:format("Restarter starts Fridge1~n"),
    process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, fridge1, [[]]),
	io:format("Pid of new fridge1 : ~p~n", [Pid]),
	receive 
		{'EXIT', Pid, normal} -> 
			io:format("Pid:~p is normal down~n", [Pid]),
			ok;
		{'EXIT', Pid, shutdown} ->
		 	io:format("Pid:~p is shutdown~n", [Pid]),
		 	ok;
		{'EXIT', Pid, Reason} ->
			io:format("Pid:~p is down with ~p~n", [Pid, Reason]),
		 	restarter()
	end.		