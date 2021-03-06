-module(rpc_tests_handler).
-behaviour(gen_server).

%% API
-export([start_link/0, add_test_result/4, get_tests/0, get_tests/1, delete_tests/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% структура для хранения состояния хранилища
%% state = atom()
-record(state, {state}).

%%%===================================================================
%%% API
%%%===================================================================

%% Функция добавляет (или заменяет) значение некоторого теста TestId для узла с номером NodeNum
add_test_result(NodeId, TestId, TestStatus, TestValue) ->
    gen_server:call(?MODULE, {add_test_result, NodeId, TestId, TestStatus, TestValue}).

%% Функция возвращает значения всех накопившихся тестов
get_tests() ->
    gen_server:call(?MODULE, {get_tests}).

%% Функция возвращает значения накопившихся тестов для узла с номером NodeNum
get_tests(NodeId) ->
    gen_server:call(?MODULE, {get_tests, NodeId}).

delete_tests(NodeId) ->
    gen_server:call(?MODULE, {delete_tests, NodeId}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    ets:new(tests, [set, named_table]),
    io:format("DEBUG>> rpc tests storage hadler started !~n"),
    {ok, #state{state=null}}.

handle_call({add_test_result, NodeId, TestId, TestStatus, TestValue}, _From, State) ->
    Key = {NodeId, TestId},
    Value = {TestStatus, TestValue},
    ets:insert(tests, {Key, Value}),
    {reply, ok, State};
    
handle_call({get_tests}, _From, State) ->
    Key = {'_', '_'},
    Values = ets:match_object(tests, {Key, '$1'}),
    {reply, {ok, Values}, State};

handle_call({get_tests, NodeId}, _From, State) ->
    Key = {NodeId, '_'},
    Values = ets:match_object(tests, {Key, '$1'}),
    {reply, {ok, Values}, State};

handle_call({delete_tests, NodeId}, _From, State) ->
    Key = {NodeId, '_'},
    Values = ets:match_delete(tests, {Key, '$1'}),
    {reply, {ok, Values}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% INTERNAL FUNCTIONS
%%%===================================================================

%get_node_tests(NodeId, []) -> not_found;
%get_node_tests(NodeId, [NodeTests | Tail]) ->
%    case NodeTests#node_tests.node_id of
%	NodeId -> NodeTests;
%	_ -> get_node_tests(NodeId, Tail)
%    end.

%get_test(TestId, []) -> not_found;
%get_test(TestId, [Test | Tail]) ->
%    case Test#test.test_id of
%	TestId -> Test;
%	_ -> get_test(TestId, Tail)
%    end.


%io:format("NodeNum    ~p~n", [NodeNum]),
%io:format("TestId     ~p~n", [TestId]),
%io:format("TestStatus ~p~n", [TestStatus]),
%io:format("TestValue  ~p~n", [TestValue]),
