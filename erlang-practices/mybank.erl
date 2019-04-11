-module(mybank).
-behaviour(gen_server).
-export([start/0]).
-export([new_account/1]).
-export([deposit/2]).
-export([withdraw/2]).
-export([stop/0]).


-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start() ->	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_account(User) ->
	gen_server:call(?MODULE, {new, User}).

deposit(User, Amount) ->
	gen_server:call(?MODULE, {deposit, User, Amount}).

withdraw(User, Amount) ->
    io:format("handle call fo withdraw.. ~n"),
	gen_server:call(?MODULE, {withdraw, User, Amount}).


stop() ->
	gen_server:call(?MODULE, stop).


	

init([]) -> {ok, ets:new(?MODULE, [])}.

handle_call({new, User}, _From, Table) ->
	Reply = case ets:lookup(Table, User) of
		[] -> ets:insert(Table, {User, 0}),
			  {welcome, User};

		[_] -> {user_already_found, User} 
	end,
	{reply, Reply, Table};

handle_call({deposit, User, Amount}, _From, Table) ->
	Reply = case ets:lookup(Table, User) of
		[] -> {not_a_customer, User};

		[{User, Balance}] -> 
		    NewBalance = Balance + Amount,
			ets:insert(Table, {User, NewBalance}),
			{ok, User, new_balance, NewBalance}
	end,
	{reply, Reply, Table};	

handle_call({withdraw, User, Amount}, _From, Table) ->	
    io:format("handle call fo withdraw ~n"),
	Reply = case ets:lookup(Table, User) of
		[] -> {not_a_customer, User};

		[{User, Balance}] -> 
		    NewBalance = Balance - Amount,
			ets:insert(Table, {User, NewBalance}),
			{ok, User, new_balance, NewBalance}
	end,
    {reply, Reply, Table};		


handle_call(stop, _From, Tab) ->
   {stop, normal, stopped,Tab}.


handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra ) -> {ok, State}.


