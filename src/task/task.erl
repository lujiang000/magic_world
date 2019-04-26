%%----------------------------------------------------
%% @doc 任务
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(task).
-export([
        login/1
        ,handle/4
        ,get_guide_task/1
        ,get_daily_task/1
        ,handle_guide/3
        ,handle_task/3
    ]
).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("animal.hrl").

%% 登陆处理,处理过期任务,初始化新手指引任务
login(Role = #role{guide_task = #guide_task{id = Id}, task = {Accept, Finish}}) ->
    Role1 = case Id of
        0 ->
            NewGuideTask = accept_guide_task(Id + 1),
            Role#role{guide_task = NewGuideTask};
        _ ->
           Role 
   end,
   NewAccept = check_task(Accept, Finish),
   Role1#role{task = {NewAccept, Finish}}.

%% 检查指引成就任务是否有新增的
check_task(Accept, Finish) ->
    List = guide_trigger_setting:get_all(),
    AcceptIds = List -- Finish,
    do_task_accept(AcceptIds, Accept).

do_task_accept([], Accept) -> Accept;
do_task_accept([Id | L], Accept) ->
    {Reward, {Type, Type1}, Target} = guide_trigger_setting:get_data(Id),
    NewAccept = case lists:keyfind(Id, #role_task.id, Accept) of
        #role_task{} -> Accept;
        _ ->
            [#role_task{id = Id, reward = Reward, type = Type, type1 = Type1, target = Target} | Accept]
    end,
    do_task_accept(L, NewAccept).





%% 推送新手务进度
push_guide_task_process(#guide_task{value = Value}) ->
    Data = #m_1706_toc{value = Value},
    sys_conn:pack_send(1706, Data).

%% 推送接受新手任务
push_new_guide_task(#guide_task{id = Id, value = Value, target = Target}) ->
    Data = #m_1705_toc{id = Id, value = Value, target = Target},
    sys_conn:pack_send(1705, Data).

%% 增加新手任务的进度
add_guide_task(Role = #role{room_pid = Pid, guide_task = Task = #guide_task{id = Id, value = Value, target = Target, reward = Reward}}, Add) ->
    push_guide_task_process(Task#guide_task{value  = Value + Add}),
    case Add + Value >= Target of
        true ->
            NewGuideTask = accept_guide_task(Id + 1),
            Pid ! {guide_task, Id + 1},
            {ok, NewRole} = role_lib:do_add(Role, Reward),
            account_mgr:output(?guide_task_coin, Reward),
            NewRole#role{guide_task = NewGuideTask};
        _ ->
            NewGuideTask  = Task#guide_task{value  = Value + Add},
            Role#role{guide_task = NewGuideTask}
    end.

%% 获取新手任务信息
get_guide_task(_Role  = #role{guide_task = #guide_task{id = Id, value = Value, target = Target}}) ->
    #m_1704_toc{id = Id, value = Value, target = Target}.

%% 获取每日任务信息
get_daily_task(#role{daily_task = {List, _}, room_type = Type}) ->
    case lists:keyfind(Type, #role_daily_task.type, List) of
        #role_daily_task{list = List1, finish = List2} ->
            #m_1707_toc{list = to_daily_task(List2 ++ List1)};
        _ ->
            []
    end.

to_daily_task(List) when is_list(List)->
    [#p_daily_task{id = Id, value = Value} ||#daily_task{id = Id, value = Value} <-List];
to_daily_task(#daily_task{id = Id, value = Value}) ->
    #p_daily_task{id = Id, value = Value}.


%% 任务入口
%% 新手任务
handle_guide(Role = #role{guide_task = #guide_task{id = Id}}, Type, Value) when Id < 6->
    do_guide_task(Role, Type, Value);
handle_guide(Role, _, _) ->  Role.

%% 每日任务
handle(Role, animal_die, Type, List) ->
    do_daily_task(Role, Type, List);
handle(Role, _Type, _RoomType, _Value) ->
    Role.

%% 触发成就任务
handle_task(Role = #role{task = {List, _Finish}}, Type, Value) ->
    do_add_task(Role, List, Type, Value,  []).

%% 增加任务进度
do_add_task(Role = #role{task = {_List, Finish}}, [Task = #role_task{id = Id, type = Type, reward = Reward} | L], Type, Value, List) -> 
    case do_add_task_process(Task, Value) of
        {ok, NewTask} ->
            do_add_task(Role, L, Type, Value, [NewTask | List]);
        finish ->
            {ok, NewRole} = role_lib:do_add(Role, Reward),
            sys_conn:pack_send(1709, #m_1709_toc{id = Id}),
            do_add_task(NewRole#role{task = {_List, [Id | Finish]}}, L, Type, Value, List)
    end;
do_add_task(Role, [Task | L], Type, Value, List) -> 
    do_add_task(Role, L, Type, Value, [Task | List]);
do_add_task(Role = #role{task = {_, Finish}}, [], _Type, _Value, List) -> 
    Role#role{task = {List, Finish}}.


do_add_task_process(#role_task{type = open_fire, type1 = Value1}, Value1) ->
    finish;
do_add_task_process(Task = #role_task{type = broke, value = Value, target = Target}, _Value1) ->
    case Value + 1  >= Target of
        true -> finish;
        _ ->
            {ok, Task#role_task{value = Value + 1}}
    end;
do_add_task_process(Task, _) -> {ok, Task}.



%% 每日任务
do_daily_task(Role = #role{daily_task = {TaskList, Time}}, Type, List) -> 
    case lists:keyfind(Type, #role_daily_task.type, TaskList) of
        #role_daily_task{list = []} -> Role;
        RoleTask = #role_daily_task{list = [Task | L], finish = Finish} -> 
            case add_daily_task(Task, List) of
                {update, Task} ->
                    Role;
                {update, NewTask} ->
                    NewRoleTask = RoleTask#role_daily_task{list = [NewTask | L]},
                    NewList = lists:keyreplace(Type, #role_daily_task.type, TaskList, NewRoleTask),
                    Role#role{daily_task = {NewList, Time}};
                {finish, NewTask = #daily_task{reward = Reward}} ->
                    animal_account_mgr:update_task(Reward),
                    {ok, NewRole} = role_lib:do_add(Role, Reward),
                    NewRoleTask = RoleTask#role_daily_task{list = L, finish = Finish ++ [NewTask]},
                    NewList = lists:keyreplace(Type, #role_daily_task.type, TaskList, NewRoleTask),
                    NewRole1 = case L of
                        [] -> 
                            task_week:handle(NewRole, daily_task, 1);
                        _ -> NewRole
                    end,
                    NewRole1#role{daily_task = {NewList, Time}}
            end;
        _ -> Role
    end.


%% 增加每日任务进度
add_daily_task(Task, []) -> 
    push_daily_task(Task),
    {update, Task};
add_daily_task(Task = #daily_task{type = elephant, value = Value, target = Target}, [#animal_base{base_id = self_elephant} | L]) ->
    case Value + 1 >= Target of
        true ->
            push_daily_task(Task#daily_task{value = Value + 1}),
            {finish, Task#daily_task{value = Value + 1}};
        _ ->
            add_daily_task(Task#daily_task{value = Value + 1}, L)
    end;
add_daily_task(Task = #daily_task{type = Type, value = Value, target = Target}, [#animal_base{base_id = Type} | L]) ->
    case Value + 1 >= Target of
        true ->
            push_daily_task(Task#daily_task{value = Value + 1}),
            {finish, Task#daily_task{value = Value + 1}};
        _ ->
            add_daily_task(Task#daily_task{value = Value + 1}, L)
    end;
add_daily_task(Task, [_ | L]) ->
    add_daily_task(Task, L).

%% 推送每日任务
push_daily_task(Task) ->
    Data = #m_1708_toc{task = to_daily_task(Task)},
    sys_conn:pack_send(1708, Data).




%% 动物击杀任务
do_guide_task(Role = #role{guide_task = #guide_task{type = animal_die, type_1 = Type}}, animal_die, AnimalList)->
    Add = case Type of
        all ->
            erlang:length(AnimalList);
        bonus ->
            erlang:length([1|| #animal_base{bonus = 1}<-AnimalList]);
        _ ->
            erlang:length([1|| #animal_base{base_id = Type1}<-AnimalList, Type1 =:= Type])
    end,
    add_guide_task(Role, Add);
%% 解锁任务
do_guide_task(Role = #role{guide_task = #guide_task{type = open_fire, type_1 = Type1}}, open_fire, Type) when Type >= Type1->
    add_guide_task(Role, 1);
%% 使用技能
do_guide_task(Role = #role{guide_task = #guide_task{type = use_skill, type_1 = Type}}, use_skill, Type)->
    add_guide_task(Role, 1);
%% 红包增加
do_guide_task(Role = #role{guide_task = #guide_task{type = red_bag}}, red_bag, Value)->
    add_guide_task(Role, Value);
do_guide_task(Role , _, _)->
    Role.



%% 接受新手指引任务
accept_guide_task(Id) ->
    case catch guide_custom_setting:get_data(Id) of
        {Reward, {Type, Type1}, Target} ->
            Task = #guide_task{id = Id, type = Type, target = Target, type_1 = Type1, reward = Reward},
            push_new_guide_task(Task),
            Task;
        _ ->
            #guide_task{id = Id}
    end.





