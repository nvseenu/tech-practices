-module(rpn).
-compile(export_all).

eval(Expression) -> 
	Tokens = string:tokens(Expression, " "),
	eval_tokens(Tokens).


eval_tokens(Tokens)  ->
	lists:foldl(fun(X, Acc) ->
		io:format("X=~p, Acc=~p ~n", [X, Acc]),
		case X of
			"+" -> 
				[V2, V1 | T] = Acc,
				[V1+V2 | T];
			"-" -> 
				[V2, V1 | T] = Acc,
				[V1-V2 | T];
			"*" -> 
				[V2, V1 | T] = Acc,
				[V1 * V2 | T];	
			Num -> 
				{V, _} = string:to_integer(Num),
				[V | Acc]
		end
	end, 
	[], Tokens).


eval1(Expression) ->
		Tokens = string:tokens(Expression, " "),
		[Res] = lists:foldl(fun eval1/2, [], Tokens),
		Res.


eval1("+", [V1, V2 | S]) -> [V2+V1 | S];
eval1("-", [V1, V2 | S]) -> [V2-V1 | S];
eval1("*", [V1, V2 | S]) -> [V2*V1 | S];
eval1(Num, Stack) ->
		R = case string:to_float(Num) of
			{error, _} -> list_to_integer(Num);
			{V, _} -> V
        end,
        [R|Stack].

