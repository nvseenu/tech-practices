-module(tcp1).
-export([start_server/0, start_client/1]).

start_server() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet ,4}, 
							 	{reuseaddr, true}, {active, true}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	gen_tcp:close(Listen),
	loop(Socket).


loop(Socket) ->	
	receive 
		{tcp, Socket, Bin} ->
			Str = binary_to_term(Bin),
			Reply = libmisc:string2value(Str),		
			ok = gen_tcp:send(Socket, term_to_binary(Reply)),
			loop(Socket);

		{tcp_closed, Socket} ->
			gen_tcp:close(Socket)
	end.
	

start_client(Message) ->
	{ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 4}]),
	ok = gen_tcp:send(Socket, term_to_binary(Message)),
	receive 
		{tcp, Socket, Bin} -> binary_to_term(Bin);
		{tcp_closed, Socket} -> gen_tcp:close(Socket)
	end.