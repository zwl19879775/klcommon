-module(fileterm).
-export([unconsult/2, consult/1]).

unconsult( File, L ) ->
	case file:open( File, write ) of
		{ok, S} ->
			lists:foreach( fun(X) ->
						io:format( S, "~p.~n", [X] ) end,
				L ),
			file:close( S ),
			{ok, 0};
		{error, Why} ->
			{error, Why}
	end.

consult( File ) ->
	case file:open( File, read ) of
		{ok, S} ->
			Val = consult1( S ),
			file:close( S ),
			{ok, Val};
		{error, Why} ->
			{error, Why}
	end.

consult1( S ) ->
	case io:read( S, '' ) of
		{ok, Term} -> [Term|consult1(S)];
		eof -> [];
		Error -> Error
	end.

