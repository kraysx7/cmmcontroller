-module(tcp_handler).
 
-export([handler/1]).

-define(TEST_RESULT_PACKET, 1).
 
-define(RECV_TIMEOUT, 20000).

%% Функция обрабатывает приходящие пакеты
handler(Socket) ->
    recvloop(Socket).

recvloop(Socket) ->
    inet:setopts(Socket,[{active,once}]),
    receive
        {tcp,Socket,Data} ->
	    %io:format("DEBUG>> Tcp data: ~w~n",[Data]),
            processdata(Data),
            recvloop(Socket);
        {tcp_closed,Socket} -> ok;
	_ -> recvloop(Socket)
    after
	?RECV_TIMEOUT -> gen_tcp:close(Socket) %20 sec timeout
    end.

processdata(<<?TEST_RESULT_PACKET:16, NodeId:32, TestId:32, TestStatus:32, ValLen:16, TestValueBin:ValLen/binary>>) ->
    rpc_tests_handler:add_test_result(NodeId, TestId, TestStatus, binary_to_list(TestValueBin)),
    io:format("RECV TEST RESULT PACKET! ~p ~p ~p ~p~n",[NodeId, TestId, TestStatus, TestValueBin]).
    
