-module(recurs).
-export([factorial/1,
	len/1, tail_fac/2,
	tail_len/2,
	duplicate/2,
	tail_duplicate/2,
	reverse/1,
	tail_reverse/1,
	sublist/2,
	tail_sublist/2,
	zip/2,
	tail_zip/2,
	quick_sort/1,
	tail_quick_sort/1,
	get_next_list/2]).



factorial(0) -> 1;
factorial(N) ->	
	N * factorial(N-1).


len([]) -> 0;
len([_ | T]) ->
	1 + len(T).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) -> tail_fac(N-1, Acc * N).


tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc + 1).


duplicate(0, _) ->	[];

duplicate(N, Term) ->
	[Term | duplicate(N-1, Term)].


tail_duplicate(N, Term) -> tail_duplicate1(N, Term, []).

tail_duplicate1(0, _Term, Acc) -> Acc;

tail_duplicate1(N, Term, Acc) ->
	Acc1 =  [Term | Acc	],
	tail_duplicate1(N-1, Term, Acc1).


reverse([]) ->  [];
reverse([H|T]) ->  reverse(T) ++ [H].

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H | Acc]).


sublist([], _) -> [];
sublist(_, 0) -> [];
sublist([H|T], N) ->
	[H | sublist(T, N-1)].


tail_sublist(L,N) -> tail_reverse(tail_sublist(L,N, [])).


tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([], _, Acc) -> Acc;
tail_sublist([H|T], N, Acc) -> tail_sublist(T, N-1, [H|Acc]).

zip([],_) -> [];
zip(_, []) -> [];
zip([H1|T1],[H2|T2]) -> [{H1, H2} | zip(T1, T2)].

tail_zip(L1, L2) -> lists:reverse(tail_zip(L1, L2, [])).

tail_zip([], _, Acc) -> Acc;
tail_zip(_,[],Acc) -> Acc;
tail_zip([H1|T1],[H2|T2], Acc) -> tail_zip(T1, T2, [{H1,H2} | Acc]).


quick_sort([]) -> [];
quick_sort([Pivot|Tail]) -> 
	{Smaller, Larger} = partition(Pivot, Tail, [], []),
	quick_sort(Smaller) ++ [Pivot] ++ quick_sort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
	if  H < Pivot ->  partition(Pivot, T, [H | Smaller], Larger);
		H >= Pivot ->  partition(Pivot,  T, Smaller, [H | Larger])
	end.

tail_quick_sort(L) -> tail_quick_sort(L, [], []).

tail_quick_sort([], _, SortedAcc) -> lists:reverse(SortedAcc);	

tail_quick_sort([Pivot|Tail] = L, Acc, SortedAcc) -> 
    io:format("L => ~p , Acc = ~p, SortedAcc = ~p ~n", [L, Acc, SortedAcc]),
	{Smaller, Larger} = partition(Pivot, Tail, [], []),
	Acc1 = [Larger | Acc],
	Acc2 = [[Pivot] | Acc1],
	Acc3 = [ Smaller | Acc2],
	%io:format("Acc3 => ~p ~n", [Acc3]),
    {NextList, NewAcc, NewSortedAcc} = get_next_list(Acc3, SortedAcc),
    %io:format("NextList => ~p , NewAcc = ~p ~n", [NextList, NewAcc]),
    tail_quick_sort(NextList, NewAcc, NewSortedAcc).	


get_next_list([], Acc) -> 
	 %   io:format("with base condition, get_next_list L: ~p, Acc=~p~n", [[], Acc]),
		{[], [], Acc};
		
get_next_list([H|T], Acc)  -> 
	%io:format("get_next_list L: ~p, T=~p, Acc=~p~n", [H, T, Acc]),
    if
    	%% Skip empty head 
    	is_list(H) ,  length(H) =:= 0 -> get_next_list(T, Acc);

    	%% Add a head with single element into Acc and proceed with Tail
    	is_list(H) , length(H) =:= 1 -> get_next_list(T, [hd(H)|Acc]);

    	%% Return the next list to be processed by quick sort and sorted result set
    	is_list(H) , length(H) > 1  -> 
    		%io:format("else part: H=~p, T=~p~n", [H, T]),
    		{H , T, Acc}
    end.
	 
