%%----------------------------------------------------
%% 日志管理进程
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(log_mgr).
-behaviour(gen_server).
-export([start_link/0
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {list = []
        ,list1 = []
    }).
-include("common.hrl").
-define(log_db_num, 12).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(db_table, [set, named_table, public]),
    List = start_log_db(?log_db_num, []),
    State = #state{list = List, list1 = []},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _}, State = #state{list = List}) ->
    NewList = case lists:keyfind(Pid, 2, List) of
        {Num , Pid} ->
            {ok, Pid1} = log_db:start_link(Num),
            lists:keyreplace(Num, 1, List, {Num, Pid1});
        _ -> List
    end,
    {noreply, State#state{list = NewList}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 启动日志处理进程
start_log_db(0, List) -> List;
start_log_db(Num, List) ->
    {ok, Pid} = log_db:start_link(Num),
    start_log_db(Num - 1, [{Num, Pid} | List]).



