-module(ex1).
-compile(export_all).


factorial(0) -> 1;

factorial(N) when N > 0 ->
	N * factorial(N-1).



