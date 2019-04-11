-module(shop).
-export([cost/1, total/1, demo/0]).
-record(user, {fname, lname}).

cost(oranges) -> 5;
cost(newspapper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.


total([]) -> 0;

total([{Item, Quantity}|Items]) -> 
	cost(Item) * Quantity + total(Items).

demo() ->
	#user{fname="srini", lname="vasan"}.




