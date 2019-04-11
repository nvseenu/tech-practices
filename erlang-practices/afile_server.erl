-module(afile_server).
-export([start/1, loop/1]).

start(Dir) ->
	spawn(afile_server, loop, [Dir]).

loop(Dir) ->
	receive 
		{Client, list_dir} -> 
			Client ! {self(), file:list_dir(Dir)};
		{Client, {get_file, Fname}} ->
			Full = filename:join(Dir, Fname),
			Client ! {self(), file:read_file(Full)}
	end,
	loop(Dir).			