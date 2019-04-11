-module(socket1).
-export([nano_get_url/0]).

nano_get_url() -> 
	nano_get_url("www.google.co.in").


nano_get_url(Host) ->
	{ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
	ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
	receive_data(Socket, []).

receive_data(Socket, Acc) ->
	receive 	
		{tcp, Socket, Bin} -> 
			receive_data(Socket, [Bin | Acc]);
		{tcp_closed, Socket} ->
			list_to_binary(lists:reverse(Acc))
	end.
