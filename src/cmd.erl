-module(cmd).
-export([run/1, run/2, run/3]).

run(Cmd) -> 
	run(Cmd, 5000).
	
run(Cmd, Timeout) ->
    io:format("Exec command: ~p~n", [Cmd]),
    Port = open_port({spawn, Cmd}, [binary, stderr_to_stdout, in, exit_status]),
    loop(Port, <<>>, Timeout).

run(Home, Cmd, Timeout) ->
    io:format("Exec command: ~p~n", [Cmd]),
    Port = open_port({spawn, Cmd}, [{cd, Home},binary, stderr_to_stdout, in, exit_status]),
    Res = loop(Port, <<>>, Timeout),
    case Res of
	timeout  -> io:format("Exec result: timeout~n~n"), timeout;
	{S, Data} -> io:format("Exec result: ~p (~p)~n~n", [S, Data]), {S, Data}
    end.
	
loop(Port, Data, Timeout) ->
	receive
		{Port, {data, NewData}} -> loop(Port, <<Data/binary, NewData/binary>>, Timeout);
		{Port, {exit_status, S}} -> {S , Data}
	after 
	    Timeout -> timeout
	end.
