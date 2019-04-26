%%----------------------------------------------------
%% 机器人管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(robot_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,start/1
        ,stop/0
        ,get_num/0
        ,web_get/0
        ,start_area_robot/2
        ,do_init/1
        ,start/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        id = 1000
        ,num = 0
        ,list = []
    }
).

-include("role.hrl").
-include("common.hrl").
-include("rank.hrl").
-record(robot_role, {
        id
        ,pid
        ,num
        ,great_num
    }
).


-define(robot_target1, [1, 2, 3, 4, 5]). %% 随机取4个 
-define(robot_target2, [6, 7, 8, 9, 10]). %% 随机取3个
-define(robot_target3, [11, 12, 13, 14, 15]). %% 随机取3个



    

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(N) ->
    ?MODULE ! {start, N}.

%% 启动竞技场机器人
start_area_robot(Id, Pid) ->
    gen_server:call(?MODULE, {start_area_robot, Id, Pid}).

do_init(List) ->
    ?MODULE ! {do_init, List}.



stop() ->
    ?MODULE ! stop,
    true.

start() ->
    ?MODULE ! start,
    true.


get_num() ->
    gen_server:call(?MODULE, get_num).


web_get() ->
    case catch gen_server:call(?MODULE, get_num) of
        {_, []} ->  [];
        {_, List} when is_list(List) ->
            [[{role_id, Id}, {kill_num, Num}, {great_num, GreatNum}, {status, is_pid(Pid)}]|| #robot_role{id = Id, num = Num, great_num = GreatNum, pid = Pid}<-List];
        _ -> []
    end.

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    erlang:process_flag(trap_exit, true),
    ets:new(robot, [named_table, set, public, {keypos, #robot_role.id}]),
    erlang:send_after(date:next_diff(3, 0, 0) * 1000, self(), zero_flush),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.


handle_call(get_num, _From, State = #state{list = List, num = Num}) ->
    {reply, {Num, List}, State};

handle_call({start_area_robot, Id, Pid}, _From, State) ->
    {ok, Pid1} = area_robot:start_link(Id, Pid),
    {reply, {ok, Pid1}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({add, Id, Pid}, State = #state{num = Num}) ->
    ets:insert(robot, #robot_role{id = Id, pid = Pid}),
    {noreply, State#state{num = Num + 1}};

handle_info({delete, Id}, State = #state{num = Num, list = List}) ->
    NewList = case lists:keyfind(Id, #robot_role.id, List) of
        Role = #robot_role{} ->
            lists:keyreplace(Id, #robot_role.id, List, Role#robot_role{pid = undefined});
        _ ->
            List
    end,
    {noreply, State#state{num = Num - 1, list = NewList}};

handle_info({start, N}, State = #state{id = Id}) ->
    [robot:start_link(Id + Num) || Num <- lists:seq(1, N)],
    {noreply, State#state{id = Id + N}};


handle_info({do_init, List}, State = #state{list = OldList}) ->
    NewList = do_init(List, []),
    NewList1 = OldList ++ NewList,
    {noreply, State#state{list = NewList1, num = erlang:length(NewList1)}};

handle_info(zero_flush, State = #state{list = List}) ->
    List1 = get_target(),
    NewList = do_change_target(List1, List),
    erlang:send_after(date:next_diff(3, 0, 0) * 1000, self(), zero_flush),
    {noreply, State#state{list = NewList}};

handle_info(start, State = #state{list = []}) ->
    List = get_target(),
    List1 = do_init(List, []),
    {noreply, State#state{list = List1, num = erlang:length(List1)}};

handle_info(start, State = #state{list = List}) ->
    List1 = [{Id, N, Num}|| #robot_role{id = Id, num = N, great_num = Num}<-List],
    List2 = do_init(List1, []),
    case List2 of
        [] -> 
            {noreply, State};
        _ ->
            {noreply, State#state{list = List2}}
    end;

handle_info(stop, State = #state{list = List}) ->
    [Pid ! stop ||#robot_role{pid = Pid} <-List],
    {noreply, State};

handle_info(over, State = #state{list = List}) ->
    [Pid ! stop ||#robot_role{pid = Pid} <-List],
    {noreply, State#state{list = []}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 关闭", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 改变机器人名次目标
do_change_target([], List) -> List;
do_change_target([{Id, N, Num} | L], List) ->
    NewList = case lists:keyfind(Id, #robot_role.id, List) of
        Role = #robot_role{pid = Pid} ->
            Pid ! {change_target,N, Num},
            lists:keyreplace(Id, #robot_role.id, List, Role#robot_role{num = N, great_num = Num});
        _ ->
            List
    end,
    do_change_target(L, NewList).



%% 启动机器人
do_init([], List) -> List;
do_init([{Id, N, Num} | L], List) -> 
    case robot:start_link({Id, N, Num}) of
        {ok, Pid} ->
            ?ERR("启动成功:~w:~w:~w", [Id,  N, Num]),
            do_init(L, [#robot_role{id = Id, pid = Pid, num = N, great_num = Num} | List]);
        _ ->
            do_init(L, List)
    end.

%% 获取所有机器人id需要拿的名次
get_animal_target() ->
    {RandList, RoleList} = get_animal_target(4, ?robot_target1, sys_env:get_env(robot), []),
    {RandList1, RoleList1} = get_animal_target(3, ?robot_target2, RandList, RoleList),
    {_RandList2, RoleList2} = get_animal_target(3, ?robot_target3, RandList1, RoleList1),
    RoleList2.

get_animal_target(0, _, RandList, List) -> {RandList, List};
get_animal_target(_, _, [], List) -> {[], List};
get_animal_target(Num, RandList, RandList1, List) ->
    N = sys_rand:rand_list(RandList),
    Id = sys_rand:rand_list(RandList1),
    get_animal_target(Num - 1, RandList -- [N], RandList1 -- [Id], [{Id, N} | List]).

%% 获取两个排行的名次
get_target() ->
    List1 = get_animal_target(),
    List2 = get_animal_target(),
    merge_target(List1, List2, []).

%% 合并名次
merge_target([], _List, List1) -> List1;
merge_target([{RoleId, N} | L], List, List1) ->
    {RoleId, Num} = lists:keyfind(RoleId, 1, List),
    merge_target(L, List, [{RoleId, N, Num} | List1]).

