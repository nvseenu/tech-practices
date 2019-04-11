-module(mysup).
-compile(export_all).

start(Children) -> ok.

stop() -> ok.

loop(Children) -> 
	receive
		_ -> loop(Children)
	end.