-module(hhfuns).
-compile(export_all).


one() -> 1.
two() -> 2.
add(F1, F2) -> F1() + F2().

map(_F, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

make_add_with(X) ->	fun(Y) -> X + Y end.


filter(_Predicate, []) -> [];

filter(Predicate, [H|T]) -> 
		case Predicate(H) of
			true -> [H | filter(Predicate, T)];
			false -> filter(Predicate, T)
		end.	


tail_filter(Predicate, L)	-> lists:reverse(tail_filter(Predicate, L, [])).

tail_filter(_Predicate, [], Acc)	-> Acc;

tail_filter(Predicate, [H|T], Acc)	->
	case Predicate(H) of
			true -> tail_filter(Predicate, T, [H|Acc]);
			false -> tail_filter(Predicate, T, Acc)
	end.


fold(_F, Start, []) -> Start;

fold(F, Start, [H|T]) ->
		R = F(H, Start),
		fold(F, R, T).

