-module(funs).
-export([head/1, second/1, tail/1, same/2, valid_time/1,right_age_to_drive/1, talk/1]).


head([H|_]) -> H.

tail([_|T]) -> T.

second([_,H|_]) -> H.


same(X,X) -> true;

same(_,_) -> false.

valid_time(Date = {_Y,_M,_D}) ->
	io:format("Date is ~p~n", [Date]);

valid_time(_) -> io:format("Invalid date format ~n").	

right_age_to_drive(X) when X >=16 , X < 90 ->
	io:format("Right age: ~p ~n", [X]);

right_age_to_drive(_) -> io:format("Not a right age ~n").	

talk(Animal) ->
	Sound = if Animal == "Cat" -> "Meow";
	   Animal == "Dog" -> "Bark";
	   Animal == "Man" -> "Hit the dog and cat"
	end,	
	{Animal, sound, Sound}.



