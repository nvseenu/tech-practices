-module(myring).
-compile(export_all).

start(N) ->
	start_proc(N, self()).

start_proc(0, Pid) ->
	Pid ! ok;

start_proc(N, Pid) ->
	NPid = spawn(?MODULE, start_proc, [N-1, Pid]),
	NPid ! ok,
	receive	ok -> ok end.