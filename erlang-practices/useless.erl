-module(useless).
-export([hello/0, add/2]).

hello() -> io:format("Hello how are you? ~n").

add(A, B) -> A + B.
