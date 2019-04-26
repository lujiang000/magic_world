%%----------------------------------------------------
%% @doc 动物园协议入口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(animal_rpc).
-export([handle/3]).

-include("common.hrl").
-include("role.hrl").
-include("animal.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

%% 进入动物园
handle(1301, #m_1301_tos{type = Type}, Role = #role{role_id = _RoleID}) ->
    case animal_mgr:enter_room(Role, Type) of
        {ok, Data, NewRole} ->
            {ok, Data, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 退出动物园
handle(1302, _, Role = #role{role_id = _RoleID}) ->
    case animal:out_room(Role) of
        {ok, NewRole} ->
            {ok, #m_1302_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 打动物
handle(1303, #m_1303_tos{id = Id, coin = Coin}, Role = #role{hit_num = Num}) ->
    case animal:hit(Role, Id, Coin) of
        {ok, NewRole} ->
            case Coin < 1000 of
                true ->
                    {ok, #m_1303_toc{}, NewRole#role{hit_num = Num + 1}};
                _ ->
                    {ok, #m_1303_toc{}, NewRole}
            end;
        {false, Reason} ->
            {false, Reason}
    end;

%% 使用道具技能
handle(1304, #m_1304_tos{type = Type}, Role) ->
    case animal:use_item(Role, Type) of
        {ok, NewRole} ->
            {ok, #m_1304_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 解锁挡位
handle(1315, _, Role = #role{use_coin = Coin}) ->
    case catch zoo_room_power_setting:get_data(Coin + 1) of
        {_Coin1, Need, Add} ->
            case role_lib:do_cost_gold(Role, Need) of
                {ok, NewRole} ->
                    {ok, NewRole1} = role_lib:do_add_coin(NewRole, Add),
                    NewRole2 = task:handle_task(NewRole1, open_fire, Coin + 1),
                    NewRole3 = role_achievement:handle(NewRole2, ?achievement_magic, Coin + 1),
                    account_mgr:output(?open_fire, Add),
                    {ok, #m_1315_toc{coin = Add}, NewRole3#role{use_coin = Coin + 1}};
                {false, Reason}->
                    {false, Reason}
            end;
        _ ->
            {false, ?error_act}
    end;

%% 获取彩金信息
handle(1316, _, _Role = #role{bonus_num = Num, bonus_pool = Pool, bonus_reward = Reward}) ->
    {reply, #m_1316_toc{bonus = Pool, num = Num, reward = max(0, Reward)}};

%% 彩金抽奖
handle(1317, _, Role) ->
    case animal:get_bonus_reward(Role) of
        {ok, Data, NewRole} ->
            NewRole1 = task_week:handle(NewRole, lottery, 1),
            {ok, Data, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end;

%% 断线重连
handle(1318, _, Role) ->
    case animal:reconnect(Role) of
        {ok, Data} ->
            {reply, Data};
        {false, Reason, NewRole} ->
            {false, Reason, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 彩金抽奖
handle(1319, _, Role) ->
    animal:push_start_bonus_reward(Role),
    ok;

%% 使用表情
handle(1321, #m_1321_tos{type = Type, to_id = Id}, Role) ->
    case animal:use_expression(Role, Type, Id) of
        {ok, NewRole} ->
            {ok, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

handle(_, _DataIn, _Role) ->
    ok.
