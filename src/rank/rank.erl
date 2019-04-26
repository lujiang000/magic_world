%%----------------------------------------------------
%% @doc 排行榜处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(rank).
-export([
        handle/2
        ,get_rank_config/1
    ]).

-include("common.hrl").
-include("role.hrl").
-include("rank.hrl").


%% 所有排行信息监听
handle(Type, Role) ->
    Flag = case Type of
        ?rank_kill ->
            case setting_mgr:get(?setting_kill_rank_on) of
                {ok, 1} -> true;
                _ -> false
            end;
        ?rank_kill_week ->
            case setting_mgr:get(?setting_kill_rank_week_on) of
                {ok, 1} -> true;
                _ -> false
            end;
        _ ->
            true
    end,
    case Flag of
        true ->
            RankRole = to_rank_info(Type, Role),
            in_rank(Type, RankRole);
        _ -> ok
    end.

% 进行上榜  玩家本身的数据
in_rank(Type, RankRole = #rank_role{id = Id, value1 = Value}) ->
    #rank_config{len = MaxLen, min = MinValue, zone = Zone} = get_rank_config(Type),
    case Value >= MinValue of
        true ->
            #rank{len = Len, last_val = Min, list = RankL} = rank_mgr:lookup(Type),
            case lists:keyfind(Id, #rank_role.id, RankL) of
                false when Value =< Min andalso Len >= MaxLen ->        %% 不在榜上
                    ok;
                _ ->
                    rank_zone:in_rank(Zone, Type, RankRole)
            end;
        _ ->
            exit_rank(Zone, Type, Id)
    end.

%%下榜
exit_rank(Zone, Type, Id) ->
    #rank{list = RankL} = rank_mgr:lookup(Type),
    case lists:keyfind(Id, #rank_role.id, RankL) of
        false -> ok;
        _ ->
            rank_zone:exit_rank(Zone, Type, Id)
    end.

%% 转换成排行数据
to_rank_info(?rank_coin, #role{role_id = RoleID, name = Name, icon = Icon, vip = Vip, coin = Coin, sign = Sign}) -> 
    #rank_role{id = RoleID, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = Coin, sign = Sign};
to_rank_info(?rank_lollipop, #role{role_id = RoleID, name = Name, icon = Icon, vip = Vip, sign = Sign, item = #role_item{lollipop = Num}}) -> 
    #rank_role{id = RoleID, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = Num, sign = Sign};
to_rank_info(?rank_kill, #role{role_id = RoleID, name = Name, icon = Icon, vip = Vip, sign = Sign, daily_kill = Kill}) -> 
    #rank_role{id = RoleID, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = Kill, sign = Sign};
to_rank_info(?rank_kill_week, #role{role_id = RoleID, name = Name, icon = Icon, vip = Vip, sign = Sign, week_kill = Kill}) -> 
    #rank_role{id = RoleID, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = Kill, sign = Sign};
to_rank_info(?rank_profit, #role{role_id = RoleID, name = Name, icon = Icon, vip = Vip, profit = Profit, sign = Sign}) -> 
    #rank_role{id = RoleID, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = Profit, sign = Sign}.


%% 获取对应的排行配置信息, 可以同一个进程处理几个榜，有需要发奖励的榜只能一个进程处理一个榜
get_rank_config(?rank_kill) ->
    #rank_config{zone = rank_zone_1, min = 1000000, len = 100};
get_rank_config(?rank_kill_week) ->
    #rank_config{zone = rank_zone_2, min = 2000000, len = 50};
get_rank_config(?rank_week_great_match) ->
    #rank_config{zone = rank_zone_3, min = 1};
get_rank_config(?rank_great_match) ->
    #rank_config{zone = rank_zone_3, min = 1};
get_rank_config(?rank_profit) ->
    #rank_config{zone = rank_zone_3, min = 100000};
get_rank_config(?rank_lollipop) ->
    #rank_config{zone = rank_zone_4, min = 1};
get_rank_config(?rank_coin) ->
    #rank_config{zone =  rank_zone_3, min = 1000000};
get_rank_config(_) ->
    #rank_config{zone = rank_zone_5}.








