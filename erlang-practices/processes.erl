-module(processes).
-export([spawn_processes/1, spawn_process/0]).

spawn_processes(N) ->
    Max = erlang:system_info(process_limit),
    io:format("Max allowed processes: ~p~n", [Max]),
    statistics(runtime),
    statistics(wall_clock),
	L = for(1,N, fun() -> spawn_process() end),
	{_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, L),
    U1 =  Time1 * 1000 / N,
    U2 =  Time2 * 1000 / N,
    io:format("process span time: ~p (~p) ms~n", [U1, U2]).


spawn_process() ->
	spawn(fun ()->
		receive 
			die -> void
		end
	end).


for(N, N, F) -> 
	[F()];

for(I, N, F) ->
	[F() | for(I+1, N, F)].



