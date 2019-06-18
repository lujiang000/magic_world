%%----------------------------------------------------
%% @doc 牧场协议入口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(farm_rpc).
-export([handle/3]).

-include("common.hrl").
-include("role.hrl").
-include("animal.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

%% 获取自己牧场信息
handle(2101, _, #role{role_id = _RoleID, farm_pid = Pid}) ->
    case farm:get_info(Pid) of
        {ok, Data} -> 
            {reply, Data};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取他人牧场信息
handle(2102, #m_2102_tos{role_id = RoleId}, _Role) ->
    case farm_mgr:get_farm_info(RoleId) of
        {ok, Name, List} ->
            {reply, #m_2102_toc{name = Name, list = List}};
        {false, Reason} ->
            {false, Reason}
    end;

%% 进入牧场
handle(2103, #m_2103_tos{role_id = RoleId, type = Type}, Role) ->
    case farm_mgr:enter(Role, RoleId, Type) of
        {ok, Data, NewRole} ->
            {ok, Data, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 退出牧场
handle(2104, _, Role) ->
    NewRole = farm_animal:out(Role),
    {ok, #m_2104_toc{}, NewRole};

%% 打动物
handle(2105, #m_2105_tos{id = Id, coin = Coin}, Role) ->
    case farm_animal:hit(Role, Id, Coin) of
        {ok, NewRole} ->
            {ok, #m_2105_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 使用道具
handle(2106, #m_2106_tos{type = Type}, Role) ->
    case farm_animal:use_item(Role, Type) of
        {ok, NewRole} -> 
            {ok, #m_2106_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 断线重连
handle(2114, _, Role) ->
    case farm_animal:reconnect(Role) of
        {ok, Data} ->
            {reply, Data};
        {false, Reason} ->
            {false, Reason}
    end;

%% 使用表情
handle(2115, #m_2115_tos{type = Type, to_id = Id}, Role) ->
    case farm_animal:use_expression(Role, Type, Id) of
        {ok, NewRole} ->
            {ok, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 解锁牧场
handle(2116, #m_2116_tos{type = Type}, Role = #role{role_id = RoleId}) ->
    case farm_mgr:open_farm(Role, Type) of
        {ok, NewRole} ->
            {ok, #m_2116_toc{type = Type}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 购买动物
handle(2117, #m_2117_tos{type = Type, list = List}, Role = #role{role_id = RoleId}) ->
    case farm:buy_animal(Role, Type, List) of
        {ok, Cost, NewRole} ->
            log_db:log(farm_buy_log, insert, [RoleId, Type, Cost, date:unixtime()]),
            {ok, #m_2117_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 领取收益
handle(2120, _, Role) ->
    case farm:get_reward(Role) of
        {ok, Reward, NewRole} ->
            {ok, #m_2120_toc{reward = Reward}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取领取收益日志
handle(2121, _, _Role = #role{farm_reward_log = List}) ->
    {reply, #m_2121_toc{list = List}};

%% 获取每日盈亏日志
handle(2122, #m_2122_tos{type = Type}, Role) ->
    case farm:get_pw_log(Role, Type) of
        {ok, List} ->
            {reply, #m_2122_toc{list = List}};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取购买日志
handle(2125, #m_2125_tos{type = Type, base_id = BaseId}, _Role = #role{role_id = RoleId}) ->
    List = farm:get_buy_log(RoleId, Type, BaseId),
    {reply, #m_2125_toc{list = List}};

%% 获取死亡日志
handle(2126, #m_2126_tos{type = Type, base_id = BaseId}, _Role = #role{role_id = RoleId}) ->
    List = farm:get_death_log(RoleId, Type, BaseId),
    {reply, #m_2126_toc{list = List}};

%% 获取动物日志
handle(2127, #m_2127_tos{type = Type}, _Role = #role{role_id = RoleId}) ->
    List = farm:get_animal_log({RoleId, Type}),
    {reply, #m_2127_toc{list = List}};
    


handle(_tos, _DataIn, _Role) ->
    ?ERR("错误的协议：~w:~w", [_tos, _DataIn]),
    {false, ?error_act}.
