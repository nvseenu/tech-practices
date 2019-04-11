-module(dog_fsm).
-compile(export_all).



start() -> spawn(fun() -> bark() end).

squirrel(Pid) -> Pid ! squirrel.
pet(Pid) -> Pid ! pet.


bark() ->
	io:format("Bark! Bark!~n"),
	receive 
		pet ->
			wag_tail();
		_ ->
			io:format("why you keep petting me?~n"),
			bark()

		after 10000 ->
			bark()
		end.


wag_tail() ->
	io:format("Wag Tail! Wag Tail!~n"),

	receive 
		pet ->
			sit();
		_ ->
			io:format("Am already wagging my tail, what do you want me to do now?"),
			wag_tail()
		after 10000 ->
			bark()
	end.		

sit() ->
	io:format("Sit! Sit!~n"),

	receive 
		squirrel ->
			bark();
		_ ->
			io:format("Am already sitting, what do you want me to do now?"),
			sit()	
	end.