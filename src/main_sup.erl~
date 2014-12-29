-module(main_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

start_link() ->
    supervisor:start_link(main_sup, []).

init(_Args) ->
    TcpListenPort = config:get(listen_port),
    {ok, {{one_for_one, 100, 1},
	  [
	   %% gen-server для обработки rpc запросов к шине i2c
	   {rpc_i2c_handler,     {rpc_i2c_handler, start_link, []}, permanent, brutal_kill, worker, []},
	   %% gen-server для приёма и отдачи результатов тестов
	   {rpc_tests_handler,   {rpc_tests_handler,   start_link, []}, permanent, brutal_kill, worker, []},
	   %% запускаем хранилище и контроллер состояния узлов
	   {nodes_state,         {nodes_state,         start_link, []}, permanent, brutal_kill, worker, []},
	   {nodes_state_cntl,    {nodes_state_cntl,    start_link, []}, permanent, brutal_kill, worker, []},
	   %% запускаем TCP сервер
	   {tcp_handler,    {tcp_serv, start_link, [[TcpListenPort, 1000, [binary, {packet, 2}], {tcp_handler, handler, []}]]}, permanent, brutal_kill, worker, []}
	  ]}}.
