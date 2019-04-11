-module(time1).
-export([login/0, dao_start/0, dao_loop/0, fetch_user_client/1]).


login() ->
	fetch_user_client(self()),
	receive 
		User -> User
	after 1000 ->
	 timedout_error	
	end.


dao_start() ->
	spawn(time1, dao_loop, []).	

dao_loop() ->
	receive 
		{From, {fetch_user}} -> 
			Res = fetch_user(),
			From ! {self(), Res}			
	end,
	dao_loop().		
	


fetch_user() ->
	timer:sleep(5000),
	{1, "srini", "vasan"}.


fetch_user_client(Pid)	 ->
	Pid ! {self(), {fetch_user}} ,
	receive
		{Pid, Res} -> Res
	after 1000 ->
		timedout_error		
	end.
			

