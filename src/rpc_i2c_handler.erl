-module(rpc_i2c_handler).
-behaviour(gen_server).

%% API
-export([start_link/0, get_i2c_info/0, get_i2c_info/1, power/1, reboot_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

get_i2c_info() ->
    gen_server:call(?MODULE, {get_i2c_info}).

get_i2c_info(NodeNum) ->
    gen_server:call(?MODULE, {get_i2c_info, NodeNum}).

power(NodeId) ->
    gen_server:call(?MODULE, {power, NodeId}).

reboot_node(NodeNum) ->
    gen_server:call(?MODULE, {reboot_node, NodeNum}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    io:format("DEBUG>> ErlRPC for I2C Server started !~n"),
    {ok, #state{}}.

handle_info(trigger, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_call({get_i2c_info}, _From, State) ->
    I2C_Command = lists:concat(["i2c1_manager -z2"]),
    Result = cmd:run(I2C_Command, 10000),
    case Result of
	{0, BinNodesInfo} -> 
	    %%Reply = {ok , [{selected_node, 3}, {temp, 45}, {boot_status, pxe}]},
	    NodesInfo = parse_nodes_info(BinNodesInfo),
	    Reply = {ok , NodesInfo},
	    {reply, Reply, State};
	timeout -> {reply, timeout, State}
    end;


handle_call({get_i2c_info, NodeId}, _From, State) ->
    I2C_Command = lists:concat(["i2c1_manager -n ",NodeId," -z2"]),
    Result = cmd:run(I2C_Command, 10000),
    case Result of
	{0, BinNodeInfo} ->
	    NodeInfo = parse_node_info(BinNodeInfo),
	    Reply = {ok , NodeInfo},
	    {reply, Reply, State};
	timeout -> {reply, timeout, State}
    end;

handle_call({power, NodeId}, _From, State) ->
    {reply, Reply, State} = handle_call({get_i2c_info, NodeId}, _From, State),
    case Reply of
	{ok, NodeInfo} -> 
	    PowerStatus = proplists:get_value(power, NodeInfo),
	    case PowerStatus of
		on  -> {reply, power(NodeId, off), State};
		off -> {reply, power(NodeId, on),  State};
		PowerStatus ->  {reply, PowerStatus, State}
	    end;
	_-> {reply, get_status_error, State}
    end;

handle_call({reboot_node, NodeNum}, _From, State)->
    Reply = {ok, NodeNum},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% INTERNAL FUNCTIONS
%%%===================================================================

power(NodeId, NewStatus) ->
    I2C_Command = lists:concat(["i2c1_manager -n ",NodeId," -p ",NewStatus]),
    Result = cmd:run(I2C_Command, 10000),
    case Result of
	{0, _} -> {ok , NodeId};
	timeout -> timeout;
	_ -> set_status_error
    end.

%% функция парсит строку вывода после вызова команды `i2c1_manager -z`
parse_nodes_info(BinNodesInfo) ->
    BinNodesInfoList = binary:split(BinNodesInfo, <<";">>, [global]),
    NodesInfo = lists:foldl(fun(BinNodeInfo, NodesInfoList) ->	  
				  lists:append(NodesInfoList, [parse_node_info(BinNodeInfo)])
			    end, [] , BinNodesInfoList),
    NodesInfo.

%% функция парсит строку вывода после вызова команды `i2c1_manager -n xxx -z`
parse_node_info(BinNodeInfo) when byte_size(BinNodeInfo) > 10->
    Values = binary:split(BinNodeInfo, <<",">>, [global]),
    NodeInfo = [
		{node_id, commons:list_to_integer(binary_to_list(lists:nth(1, Values)))},
		{node_status, binary_to_atom(lists:nth(2, Values), latin1)},
		{power, binary_to_atom(lists:nth(3, Values), latin1)},
		{com_line, binary_to_atom(lists:nth(4, Values), latin1)},
		{usb_fase_in, binary_to_atom(lists:nth(5, Values), latin1)},
		{wdt_state, binary_to_atom(lists:nth(6, Values), latin1)},
		{wdt_mode, binary_to_atom(lists:nth(7, Values), latin1)},
		{ampere, commons:list_to_float(binary_to_list(lists:nth(8, Values)))},
		{volt_io_cpu_ports, commons:list_to_float(binary_to_list(lists:nth(9, Values)))},
		{volt_io_dimm, commons:list_to_float(binary_to_list(lists:nth(10, Values)))},
		{volt_cores, commons:list_to_float(binary_to_list(lists:nth(11, Values)))},
		{volt_cpu, commons:list_to_float(binary_to_list(lists:nth(12, Values)))},
		{temperature, commons:list_to_float(binary_to_list(lists:nth(13, Values)))}
	       ],
    NodeInfo;

parse_node_info(_) -> [].
