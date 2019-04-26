%%----------------------------------------------------
%% 功能管理系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(functions_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,get_exchange_flow/0
        ,add_exchange_flow/1
        ,delete_exchange_flow/1
        ,reload/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        exchange_flow = 0         %% 剩余兑换流量
    }
).

-include("common.hrl").
-include("error_msg.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 获取兑换流量
get_exchange_flow() ->
    case catch gen_server:call(?MODULE, get_exchange_flow) of
        {ok, Flow} ->
            Flow;
        _ ->
            0
    end.

%% 增加兑换流量
add_exchange_flow(Value) when is_integer(Value) ->
    case catch gen_server:call(?MODULE, {add_exchange_flow, Value}) of
        {ok, Flow} ->
            {ok, Flow};
        _ ->
            {false, ?error_busy}
    end.

%% 扣除兑换流量
delete_exchange_flow(Value) when is_integer(Value) ->
    ?MODULE ! {delete_exchange_flow, Value}.

%% 重载数据库数据
reload() ->
    ?MODULE ! reload.
    

init([]) ->
    Flow = do_init(),
    State = #state{exchange_flow = Flow},
    {ok, State}.

%% 获取兑换流量
handle_call(get_exchange_flow, _From, State = #state{exchange_flow = Flow}) ->
    {reply, {ok, Flow}, State};

%% 增加兑换流量
handle_call({add_exchange_flow, Value}, _From, State = #state{exchange_flow = Flow}) ->
    db_save(Flow + Value),
    {reply, {ok, Flow + Value}, State#state{exchange_flow = Flow + Value}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 扣除流量
handle_info({delete_exchange_flow, Value}, State = #state{exchange_flow = Flow}) ->
    db_save(Flow - Value),
    {noreply, State#state{exchange_flow = Flow - Value}};

%% 重载流量数据
handle_info(reload, State) ->
    Flow = do_init(),
    {noreply, State#state{exchange_flow = Flow}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 初始化，获取兑换流量
do_init() ->
    case db:get_one("select flow from exchange_flow where id = 1") of
        {ok, Flow} when is_integer(Flow)->
            Flow;
        {ok, undefined} ->
            Default = 10000,
            db:exec("insert into exchange_flow (id, flow) value (1, ?)", [Default]),
            Default;
        _Err ->
            ?ERR("兑换流量读取数据库出错", [_Err]),
            0
    end.

%% 储存到数据库
db_save(Value) ->
    db:exec("replace into exchange_flow (id, flow) value (1, ?)", [Value]).

