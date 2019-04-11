-module(exceptions).
-compile(export_all).


throws(F) ->
		try F() of
			_ -> ok
		catch
			Throw -> {throw, caught, Throw}
	    end.


error(F) ->
		try F() of
			_ -> ok
		catch 
			error:Error -> {error, caught, Error}
		end.		    

