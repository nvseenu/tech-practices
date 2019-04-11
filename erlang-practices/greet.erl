-module(greet).
-export([greet/2]).


find_prefix(male) ->
	"Mr.";

find_prefix(female) ->
	"Mrs.";


find_prefix(_Gender) ->
	"".	

greet(Gender, Name) ->
	io:format("Hello ~s~s~n", [find_prefix(Gender), Name]).
