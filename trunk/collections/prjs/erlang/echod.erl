%
% an echo server sample
%
-module(echod).
-export([start/0, start/1]).

service( Socket ) ->
	receive
		go_ahead ->
			% receive data using signal
			inet:setopts( Socket, [{active, true}] ),
			gen_tcp:send( Socket, "\nErlang echo server!\n\n" ),
			service( Socket );
		{tcp, Socket, Bin} ->
			% received data
			%io:format( "receive data from ~p : ~p ~n", [Socket, Bin] ),
			gen_tcp:send( Socket, Bin ),
			service( Socket );
		{tcp_closed, Socket} ->
			% terminate this process
			io:format( "client ~p disconnect~n", [Socket] ),
			exit( closed );
		{tcp_error, Socket, Reason} ->
			io:format( "socket error ~p~n", [Reason] ),
			exit( closed )
	end.

loop( S ) ->
	{ok, Socket} = gen_tcp:accept( S ),
	Pid = spawn( fun() -> service( Socket ) end ),
	% link the socket to the Process, the socket will be closed
	% when the process died.
	gen_tcp:controlling_process( Socket, Pid ),
	Pid ! go_ahead,
	loop( S ).

start() ->
	start( 3124 ).

start( Port ) ->
	% spawn a new process so that it will not block the erl shell
	spawn( fun() ->
		{ok, S} = gen_tcp:listen( Port, [{packet, 0}, {active, false}] ),
		% make it to be system process, when client process died, it will
		% not died.
		process_flag( trap_exit, true ),
		loop( S ) end ).

