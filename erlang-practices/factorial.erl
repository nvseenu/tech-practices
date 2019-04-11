-module(factorial).
-compile(export_all).

tc(Num) ->
    {T, _} = timer:tc(?MODULE, factorial, [Num]),
    T.

factorial(Num) -> fact(Num, 1).

fact(0, Acc) -> Acc;

fact(Num, Acc) ->
    Acc1 = Acc * Num,
    fact(Num-1, Acc1).