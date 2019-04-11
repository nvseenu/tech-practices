-module(geometry_server).
-behaviour(gen_server).
-compile(export_all).

%%  interface functions
start_link() -> gen_server:start_link(?MODULE, [], []).

square_area(Pid, Side1, Side2) ->
	gen_server:call(Pid, {square, Side1, Side2}).


%% callback fundtions
init([]) -> {ok, []}.

handle_call({square, Side1, Side2}, _From, State) ->
	{reply, Side1*Side2, State}.


handle_cast(_Msg, State) ->
	  {noreply, State}.

handle_info(Msg, State) -> 
	io:format("unexpected messge : ~p~n", [Msg]),
	{noreply, State}.	

