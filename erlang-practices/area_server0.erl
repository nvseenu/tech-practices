-module(area_server0).
-export([loop/0]).

loop() ->
	receive 
		{rectangle, Width, Height} -> 
			io:format("Rect area ~p~n", [Width * Height]),
			loop();

		{square, Side} -> 
			io:format("Square area ~p~n", [Side * Side]),
			loop()
    end,
    loop().
		
