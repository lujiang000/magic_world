%%----------------------------------------------------
%% 全服自动增长id管理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(auto_increment).
-behaviour(gen_server).
-export([start_link/0
        ,get_auto_id/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("auto_increment.hrl").
-include("common.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 获取自动增长id
get_auto_id(Key = role) ->
    Num = sys_rand:rand(1, 5),   %% 随机增长id
    ets:update_counter(auto_increment, Key, Num);
get_auto_id(Key) ->
    ets:update_counter(auto_increment, Key, 1).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(auto_increment, [public, named_table, set, {keypos, #auto_increment.key}]),
    do_init(?auto_key_list),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, #state{}}.

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



do_init([]) -> ok;
do_init([#auto_config{key = Key, db_name = Name, value = Value, define = Define} | L]) ->
    Sql = io_lib:format("select max(~w) from ~w", [Value, Name]),
    case db:get_one(Sql) of
        {ok, Num} when is_integer(Num) ->
            ets:insert(auto_increment, #auto_increment{key = Key, value = Num});
        {ok, undefined} ->
            ets:insert(auto_increment, #auto_increment{key = Key, value = Define});
        _Err -> 
            ?ERR("初始化自动增长id出错：~w  reason:~w", [Key, _Err]),
            ok
    end,
    do_init(L).




