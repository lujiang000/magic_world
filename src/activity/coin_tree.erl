%%----------------------------------------------------
%% @doc 摇钱树获得
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(coin_tree).
-export([
        get_info/1
        ,shake/1
    ]
).


-include("common.hrl").
-include("role.hrl").
-include("error_msg.hrl").

-define(start_time, date:datetime_to_seconds({2018, 11, 5, 0, 0, 0})).
-define(end_time, date:datetime_to_seconds({2018, 11, 19, 0, 0, 0})).


%% 获取摇钱树信息
get_info(Role = #role{coin_tree_time = Time}) ->
    N = role_lib:get_value(Role, ?daily_coin_treee),
    Now = date:unixtime(),
    {N, max(0, Time - Now)}.



%% 摇钱树
shake(Role = #role{coin_tree_time = Time, vip = Vip}) ->
    Now = date:unixtime(),
    case setting_mgr:get(?setting_coin_tree) of
        {ok, [Start, End]} when Now >= Start andalso Now < End ->
            N = role_lib:get_value(Role, ?daily_coin_treee),
            Max = case Vip =:= 0 of
                true ->
                    3;
                _ ->
                    5
            end,
            case N < Max of
                true ->
                    case N of
                        0 ->
                            do_shake(Role, N, Now);
                        _ ->
                            case Now >= Time of
                                true ->
                                    do_shake(Role, N, Now);
                                _ ->
                                    {false, ?error_coin_tree_reward_time}
                            end
                    end;
                _ ->
                    {false, ?error_coin_tree_num}
            end;
        _ ->
            {false, ?error_coin_tree_time}
    end.

do_shake(Role = #role{role_id = RoleId}, N, Time) ->
    {NeedCoin, WaitTime} = get_coin_tree(N),
    case role_lib:do_cost_coin(Role, NeedCoin) of
        {ok, NewRole} ->
            AddCoin = do_shake1(NeedCoin),
            {ok, NewRole1} = role_lib:do_add_coin(NewRole, AddCoin),
            NewRole2 = role_lib:add_value(NewRole1, ?daily_coin_treee),
            log_db:log(coin_tree_log, insert, [RoleId, NeedCoin, AddCoin, date:unixtime()]),
            {ok, AddCoin, NewRole2#role{coin_tree_time = Time + WaitTime}};
        {false, Reason} ->
            {false, Reason}
    end.


do_shake1(Coin) ->
    List1 = [{1, 544}, {6, 5000}, {8, 8000}, {10, 4000}, {12, 8000}, {20, 2000}, {5000, 2}],
    {Num, _} = sys_rand:rand_list(List1, 2),
    Add = erlang:trunc(Coin * Num/100),
    Coin + Add.



%% 投入金币，等待时间秒
get_coin_tree(0) -> {20000, 10 * 60};
get_coin_tree(1) -> {30000, 20 * 60};
get_coin_tree(2) -> {50000, 30 * 60};
get_coin_tree(3) -> {100000, 60 * 60};
get_coin_tree(4) -> {200000, 0}.




