%%----------------------------------------------------
%% @doc 每周任务
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(task_week).
-export([
        init/2
        ,get_info/1
        ,get_reward/2
        ,handle/3
    ]
).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("animal.hrl").
-include("error_msg.hrl").

-record(week_task, {
        id = 0
        ,type = 0
        ,type1 = 0
        ,target = 0
        ,value = 0
        ,reward_list = []
        ,reward = 0
    }
).

%% 初始化
init(Role = #role{week_task = List1}, Day) when Day =:= 1 orelse List1 =:= []->
    List = mission_weekly_setting:get_all(),
    NewList = do_init(List, []),
    Role#role{week_task = NewList};
init(Role, _) ->
    Role.

do_init([], List) -> List;
do_init([N | L], List) -> 
    {Target,Reward,{Type,Type1}} = mission_weekly_setting:get_data(N),
    Task = #week_task{id = N, type = Type, target = Target, reward_list = Reward, type1 = Type1},
    do_init(L, [Task | List]).

%% 获取信息
get_info(#role{week_task = List}) ->
    [#p_week_task{id = Id, value = Value, reward = Reward} ||#week_task{id = Id, value = Value, reward = Reward} <-List].

%% 领取奖励
get_reward(Role = #role{week_task = List}, Id) ->
    case lists:keyfind(Id, #week_task.id, List) of
        Task = #week_task{reward_list = Reward, reward = 0, value = Value, target = Target} when Value >= Target->
            {ok, NewRole} = role_lib:do_add(Role, Reward),
            NewList = lists:keyreplace(Id, #week_task.id, List, Task#week_task{reward = 1}),
            {ok, NewRole#role{week_task = NewList}};
        _ ->
            {false, ?error_act}
    end.

%%get_data(1) -> {5,[{gold,10}],{sign,1}};
%%get_data(2) -> {15,[{gold,10}],{share,1}};
%%get_data(3) -> {5,[{gold,10}],{lottery, 1}};
%%get_data(4) -> {100,[{coin,60000}],{animal_die, lion}};
%%get_data(5) -> {100,[{coin,80000}],{animal_die, elephant}};
%%get_data(6) -> {1000,[{coin,20000}],{animal_die, all}};
%%get_data(7) -> {5000000,[{coin,50000}],{win_gold,1}};
%%get_data(8) -> {5,[{coin,20000}],{daily_task,1}}.

%% 任务触发
handle(Role = #role{week_task = List}, Type, Value) ->
    NewList = do_handle(List, Type, Value, []),
    Role#role{week_task = NewList}.

do_handle([Task = #week_task{type = Type, value = Old, target = Target} | L], Type, Value, List) when Old < Target->
    NewTask = do_task(Task, Type, Value),
    do_handle(L, Type, Value, [NewTask | List]);
do_handle([Task | L], Type, Value, List) ->
    do_handle(L, Type, Value, [Task | List]);
do_handle([], _Type, _Value, List) -> List.


do_task(Task = #week_task{type1 = Type, value = Old, target = Target}, animal_die, List) ->
    Add = case Type of
        all ->
            erlang:length(List);
        bonus ->
            erlang:length([1|| #animal_base{bonus = 1}<-List]);
        elephant ->
            erlang:length([1|| #animal_base{base_id = Type1}<-List, (Type1 =:= Type orelse Type1 =:= self_elephant)]);
        _ ->
            erlang:length([1|| #animal_base{base_id = Type1}<-List, Type1 =:= Type])
    end,
    Task#week_task{value = min(Old + Add, Target)};
do_task(Task = #week_task{value = Old, target = Target}, win_gold, List) ->
    case lists:keyfind(coin, 1, List) of
        {coin, Value} ->
            Task#week_task{value = min(Old + Value, Target)};
        _ ->
            Task
    end;
do_task(Task = #week_task{value = Old, target = Target}, _, Value) ->
    Task#week_task{value = min(Old + Value, Target)}.







