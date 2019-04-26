%%----------------------------------------------------
%% @doc 人物成就
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_achievement).
-export([
        login/1
        ,handle/3
        ,reward/3
        ,get_info/1
    ]
).

-include("common.hrl").
-include("role.hrl").
-include("error_msg.hrl").
-include("all_pb.hrl").

-record(role_achievement, {
        type = 0
        ,id = 1            %% 当前成就id
        ,reward_list = []  %% 可以领取的奖励列表
        ,value = 0         %% 当前成就值
    }
).


%% 登陆处理
login(Role = #role{achievement = List}) ->
    NewList = do_init(?achievement_type_list, List),
    Role#role{achievement = NewList}.

%% 初始化
do_init([Type | L], List) -> 
    NewList = case lists:keyfind(Type, #role_achievement.type, List) of
        #role_achievement{} -> 
            List;
        _ -> 
            [#role_achievement{type = Type} | List]
    end,
    do_init(L, NewList);
do_init([], List) -> List.

%% 成就触发
handle(Role = #role{achievement = List}, Type, Value) ->
    case lists:keyfind(Type, #role_achievement.type, List) of
        Ach = #role_achievement{} ->
            NewAch = do_update(Ach, Type, Value),
            Role#role{achievement = lists:keyreplace(Type, #role_achievement.type, List, NewAch)};
        _ ->
            Role
    end.

%% 领取奖励
reward(Role = #role{achievement = List}, Type, Id) ->
    case lists:keyfind(Type, #role_achievement.type, List) of
        Ach = #role_achievement{reward_list = IdList} ->
            case lists:member(Id, IdList) of
                true -> 
                    {_, ItemType, Num} = get_data(Type, Id),
                    {ok, NewRole} = role_lib:do_add(Role, [{ItemType, Num}]),
                    NewAch = Ach#role_achievement{reward_list = lists:delete(Id, IdList)},
                    NewRole1 = NewRole#role{achievement = lists:keyreplace(Type, #role_achievement.type, List, NewAch)},
                    {ok, NewRole1};
                _ -> {false, ?error_act}
            end;
        _ ->
            {false, ?error_act}
    end.

%% 获取成就信息
get_info(#role{achievement = List}) ->
    [#p_achievement{type = Type, id = Id, reward_list = List1, value = Value}|| #role_achievement{type = Type, id = Id, reward_list = List1, value = Value}<-List].

%% 根据类型和id获取成就数据
get_data(_Type = ?achievement_hunt, Id) ->
    catch achievement_hunt_setting:get_data(Id);
get_data(_Type = ?achievement_login, Id) ->
    catch achievement_login_setting:get_data(Id);
get_data(_Type = ?achievement_magic, Id) ->
    catch achievement_magic_setting:get_data(Id).


%% 更新成就
do_update(Ach = #role_achievement{id = Id, reward_list = List}, Type = ?achievement_magic, Value) ->
    case get_data(Type, Id) of
        {Target, _, _} ->
            case Value >= Target of
                true ->
                    NewAch = Ach#role_achievement{id = Id + 1, reward_list = [Id | List], value = Value},
                    sys_conn:pack_send(1156, #m_1156_toc{type = Type, id = Id}),
                    do_update(NewAch, Type, Value);
                _ ->
                    Ach#role_achievement{value = Value}
            end;
        _ -> Ach
    end;
do_update(Ach = #role_achievement{id = Id, reward_list = List, value = Old}, Type, Value) ->
    case get_data(Type, Id) of
        {Target, _, _} ->
            case Value + Old >= Target of
                true ->
                    NewAch = Ach#role_achievement{id = Id + 1, reward_list = [Id | List], value = Value + Old},
                    sys_conn:pack_send(1156, #m_1156_toc{type = Type, id = Id}),
                    do_update(NewAch, Type, 0);
                _ ->
                    Ach#role_achievement{value = Value + Old}
            end;
        _ -> Ach
    end.






