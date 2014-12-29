-module(nodes_state).
-behaviour(gen_server).

%% API
-export([start_link/0, get_state/2, set_state/3]).
-export([get_i2c_state/0, get_i2c_state/1, update_i2c_state/0, bootmode/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(UPDATE_INTERVAL, 1000). % 1 second

%% структура для хранения состояния хранилища
%% state := atom()
-record(state, {state}).

%%%===================================================================
%%% API
%%%===================================================================

get_state(NodeId, StateName) ->
    gen_server:call(?MODULE, {get_state, NodeId, StateName}).

set_state(NodeId, StateName, Value) ->
    gen_server:call(?MODULE, {set_state, NodeId, StateName, Value}).

get_i2c_state() ->
    gen_server:call(?MODULE, {get_i2c_state}).

get_i2c_state(NodeId) ->
    gen_server:call(?MODULE, {get_i2c_state, NodeId}).

update_i2c_state() ->
    gen_server:call(?MODULE, {update_i2c_state}).

bootmode(NodeId, Mode) ->
   gen_server:cast(?MODULE, {bootmode, NodeId, Mode}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    ets:new(nodes_state, [set, named_table]),
    io:format("DEBUG>> nodes_state handler/cache started !~n"),
    {ok, #state{state=null}}.

handle_call({get_state, NodeId, StateName}, _From, State) ->
    Key = {NodeId, StateName},
    Values = ets:match_object(nodes_state, {Key, '$1'}),
    {reply, {ok, Values}, State};

handle_call({set_state, NodeId, StateName, Value}, _From, State) ->
    Key = {NodeId, StateName},
    ets:insert(nodes_state, {Key, Value}),
    {reply, {ok, Value}, State};

handle_call({get_i2c_state}, _From, State) ->
    Key = {'_', i2c},
    Values = ets:match_object(nodes_state, {Key, '$1'}),
    {reply, {ok, Values}, State};

handle_call({get_i2c_state, NodeId}, _From, State) ->
    Key = {NodeId, i2c},
    Values = ets:match_object(nodes_state, {Key, '$1'}),
    {reply, {ok, Values}, State};

handle_call({update_i2c_state}, _From, State) ->
    {ok, NodesInfo} = rpc_i2c_handler:get_i2c_info(),
    update_i2c_state_(NodesInfo),
    {reply, ok, State};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({bootmode, NodeId, Mode}, State) ->
    I2C_Command = lists:concat(["bootmode ", NodeId, " ", Mode]),
    cmd:run(I2C_Command),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_trigger, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% INTERNAL FUNCTIONS
%%%===================================================================

update_i2c_state_([]) -> ok;
update_i2c_state_([NodeInfo | Other]) ->
    NodeId = proplists:get_value(node_id, NodeInfo),
    case NodeId of
	NodeId when is_integer(NodeId) -> 
	    Key = {NodeId, i2c},
	    ets:insert(nodes_state, {Key, NodeInfo});
	_-> ok
    end,
    update_i2c_state_(Other).
