%%----------------------------------------------------
%% @doc 活动协议入口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(activity_rpc).
-export([
        handle/3
        ,get_active_list/2
    ]
).
-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

-define(active_coin_tree, 1).
-define(active_new_year, 2).
-define(active_gold_pick, 3).
-define(active_girl, 4).
-define(activity_list, [?active_coin_tree, ?active_new_year, ?active_gold_pick, ?active_girl]). %% 需要判断的开启的活动

%% 获取摇钱树信息
handle(1901, _, Role) ->
    {Num, Time} = coin_tree:get_info(Role),
    {reply, #m_1901_toc{num = Num, time = Time}};


%% 领取摇钱树奖励
handle(1902, _, Role) ->
    case coin_tree:shake(Role) of
        {ok, Coin, NewRole} ->
            {ok, #m_1902_toc{coin = Coin}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取活动信息列表 
handle(1903, _, _Role) ->
    List = get_active_list(?activity_list, []),
    {reply, #m_1903_toc{list = List}};

%% 获取活动礼包购买次数
handle(1904, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_girl_active),
    {reply, #m_1904_toc{num = Num}};

%% 活动转盘 
handle(1905, _, Role) ->
     case do_active_turn(Role) of
         {ok, Info, NewRole} ->
             {ok, #m_1905_toc{num = Info}, NewRole};
         {false, Reason} ->
             {false, Reason}
     end;


handle(_Cmd, _Data, _Role) ->
    ?ERR("错误的协议数据cmd:~w,data:~w", [_Cmd, _Data]),
    ok.


get_active_list([Type | L], List) -> 
    SettingType = case Type of
        ?active_coin_tree -> ?setting_coin_tree;
        ?active_new_year -> ?setting_new_year;
        ?active_girl -> ?setting_girl;
        ?active_gold_pick -> ?setting_gold_pick
    end,
    Now = date:unixtime(),
    NewList = case setting_mgr:get(SettingType) of
        {ok, [Start, End]} when Now >= Start andalso Now < End ->
            [#p_active{type = Type, start_time = Start, end_time = End} | List];
        _ ->
            List
    end,
    get_active_list(L, NewList);
get_active_list([], List) -> List.


%% 活动转盘
do_active_turn(Role = #role{name = Name}) ->
    case role_lib:do_cost(Role, [{active_card, 38}]) of
        {ok, NewRole} ->
            List = [{coin, 380000, 5}, {coin, 20000, 150}, {gold, 50, 20}, {gold, 20, 150}, {candy, 1, 20}, {lollipop, 1, 1}, {lolly, 1, 4}, {gold_pick, 1, 10}],
            {Type, Num, _} = sys_rand:rand_list(List, 3),
            {ok, NewRole1} = role_lib:do_add(NewRole, [{Type, Num}]),
            Data = #p_assets{type = Type, num = Num},
            case Num =:= 380000 of
                true -> 
                    Msg = util:fbin("恭喜玩家 ~ts 在幸运大抽奖中获得【380000金币】", [Name]),
                    boradcast_mgr:barrage(Msg);
                _ ->
                    case Type =:= lollipop of
                        true -> 
                            Msg = util:fbin("恭喜玩家 ~ts 在幸运大抽奖中获得【一个波板糖】", [Name]),
                            boradcast_mgr:barrage(Msg);
                        _ -> ok
                    end
            end,
            {ok, Data, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end.


