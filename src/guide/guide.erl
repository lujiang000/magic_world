%%----------------------------------------------------
%% @doc 新手指引
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(guide).
-export([
        get_guide_gift/1
        ,reward_guide_gift/1
    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

%% 获取新手礼包领取情况
get_guide_gift(_Role = #role{guide_gift = Day, guide_gift_time = Time}) ->
    case Day >= 2 of
        true ->
            #m_1803_toc{flag = 0};
        _ ->
            Now = date:unixtime(),
            case date:is_same_day(Now, Time) of
                true ->
                    #m_1803_toc{flag = 0};
                _ ->
                    #m_1803_toc{flag = 1, day = Day + 1}
            end
    end.


%% 领取新手礼包 
reward_guide_gift(Role = #role{guide_gift = Day, guide_gift_time = Time}) ->
    case Day >= 2 of
        true ->
            {false, ?error_act};
        _ ->
            Now = date:unixtime(),
            case date:is_same_day(Now, Time) of
                true ->
                    {false, ?error_act};
                _ ->
                    Reward = get_day_reward(Day),
                    {ok, NewRole} = role_lib:do_add(Role, Reward),
                    account_mgr:output(?guide_login_coin, Reward),
                    {ok, NewRole#role{guide_gift = Day + 1, guide_gift_time = Now}}
            end
    end.


%% 获取新手礼包信息
get_day_reward(0) -> [{coin, 10000}, {locking, 2}, {tel_fare, 10}, {gold, 2}];
get_day_reward(1) -> [{tel_fare, 20}, {coin, 4000}, {gold, 10}, {locking, 5}].
