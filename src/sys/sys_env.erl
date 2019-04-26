%%----------------------------------------------------
%%  服务器环境变量管理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(sys_env).
-behaviour(gen_server).
-export([start_link/0
        ,reload/0
        ,set/2
        ,get_env/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 重载配置
reload() ->
    Root = get_env(root),
    {ok, [{_, _, Data}]} = file:consult(Root ++ "/main.app"),
    case lists:keyfind(env, 1, Data) of
        {env, List} ->
            ?INFO("env config  reload success", []),
            [ets:insert(ets_env, {Key, Value})||{Key, Value} <-List];
        _ -> ok
    end.

%% 设置变量
set(Key, Value) ->
    ets:insert(ets_env, {Key, Value}).

%% 获取变量 性能还可以，暂时先不用进程字典缓存，因为重载了之后进程字典还是拿的旧数据
get_env(Key) ->
    case ets:lookup(ets_env, Key) of
        [{Key, Value}] -> Value;
        _ -> undefined
    end.

init([]) ->
    ets:new(ets_env, [named_table, public, set, {read_concurrency, true}, {keypos, 1}]),    %%基本只是读取所以可以提速
    Root = filename:absname(""),
    ets:insert(ets_env, {root, Root}),
    List = application:get_all_env(),
    [ets:insert(ets_env, {Key, Value})||{Key, Value} <-List],
    State = #state{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




