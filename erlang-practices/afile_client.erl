-module(afile_client).

-export([ls/1, get_file/2]).

ls(Server) ->
		Server ! {self(), list_dir},
		receive  
			{Server, Files} -> Files
		end.


get_file(Server, FName) ->
		Server ! {self(), {get_file, FName}},
		receive 
			{Server, Content} ->
					Content
		end.							

