-module(nodes_state_cntl).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(UPDATE_INTERVAL, 1000). % 1 second


-record(state, {}).
%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start update timer
    erlang:send_after(?UPDATE_INTERVAL, self(), update_trigger),
    io:format("DEBUG>> nodes_state handler/cache started !~n"),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_trigger, State) ->
    %% получаем текущие данные состояний i2c
    {ok, CurI2C} = nodes_state:get_i2c_state(),
    %% обновляем хранилище состояния i2c
    nodes_state:update_i2c_state(),
    %% получаем обновлённые данные
    {ok, NewI2C} = nodes_state:get_i2c_state(),
    %% идём по новым данным и сравниваем со старыми
    lists:foreach(fun(Elem) ->
			  {{NodeId, i2c}, _} = Elem,
			  check_i2c_state(Elem, get_state({NodeId, i2c}, CurI2C))
		  end, NewI2C),
    %% Start new timer
    erlang:send_after(?UPDATE_INTERVAL, self(), update_trigger),
    {noreply, State};


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_i2c_state([], _) -> ok;
check_i2c_state(_, []) -> ok;
check_i2c_state(NewState, OldState) ->
    {{NodeId, _}, New} = NewState,
    {_, Old} = OldState,
    Status = proplists:get_value(node_status, New),
    PowerN = proplists:get_value(power, New),
    %%NodeStatusO = proplists:get_value(node_status, Old),
    PowerO = proplists:get_value(power, Old),
    case {PowerO, PowerN, Status} of
	{_, _, disable} -> rpc_tests_handler:delete_tests(NodeId);
	{_, off, _} -> rpc_tests_handler:delete_tests(NodeId);
	_ -> ok
    end.

get_i2c_state(NodeId, StatesList) ->
    get_state({NodeId, i2c}, StatesList).

%% ф-я получает значение некоторого состояния узла по ключу из списка состояний
%% Key = {NodeId, statename}
get_state(_Key, []) -> [];
get_state(Key, [State | Other]) ->
    case State of
	{Key, _} -> State;
	_ -> get_state(Key, Other)
    end.





    
    
    
    
