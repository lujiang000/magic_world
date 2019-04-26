%%----------------------------------------------------
%% @doc 人物相关处理工具
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_lib).
-export([
        check_id_card/1
        ,do_cost/2
        ,do_add/2
        ,do_cost_coin/2
        ,do_cost_gold/2
        ,do_cost_lollipop/2
        ,do_add_coin/2
        ,do_add_gold/2
        ,get_bag_info/1
        ,use_item/3
        ,use_trumpet/2
        ,send_buff_begin/0
        ,send_buff_flush/0
        ,send_buff_clean/0
        ,do_send_log/2
        ,do_use_log/2
        ,do_exchange_log/2
        ,get_value/2
        ,get_value/3
        ,set_value/3
        ,add_value/3
        ,add_value/2
        ,do_login_reward/1
        ,do_vip_login_reward/1
        ,do_alms_reward/1
        ,set_role/3
        ,reward_red_bag/1
        ,check_red_bag/1
        ,do_set_sheep/1
    ]

).

-include("role.hrl").
-include("common.hrl").
-include("error_msg.hrl").
-include("all_pb.hrl").

%% 身份证验证用的
-define(value_list, [7,9,10,5,8,4,2,1,6,3,7,9,10,5,8,4,2]).


do_set_sheep(Role = #role{animal_flag = 99}) -> 
    Role;
do_set_sheep(Role = #role{animal_flag = 97}) -> 
    Role;
%% 处理薅羊毛的
do_set_sheep(Role = #role{animal_flag = Flag, charge = Charge, exchange = Exchange, regist_time = Time, item = #role_item{red_bag = Red}}) -> 
    case date:day_diff(date:unixtime(), Time) of
        0 ->
            case Red + erlang:trunc(Exchange/10) >= 30 andalso Charge =< 1000 of
                true -> 
                    Role#role{animal_flag = 98};
                _ ->
                    case Flag of
                        98 ->
                            Role#role{animal_flag = 0};
                        _ ->
                            Role
                    end
            end;
        Day ->
            case Charge/Day < 100 of
                true ->
                    Role#role{animal_flag = 98};
                _ ->
                    case Flag of
                        98 ->
                            Role#role{animal_flag = 0};
                        _ ->
                            Role
                    end
            end
    end.


%% 设置人物信息
%% 名字
set_role(Role, 1, Name) ->
    case util:name_len_valid(Name, 1, 20) of
        true ->
            case do_cost_gold(Role, 500) of
                {ok, NewRole} ->
                    {ok, NewRole#role{name = Name}};
                {false, Reason}->
                    {false, Reason}
            end;
        _ ->
            {false, ?error_msg}
    end;
%% 签名
set_role(Role, 2, Sign) ->
    case util:name_len_valid(Sign, 0, 100) of
        true ->
            {ok, Role#role{sign = Sign}};
        _ ->
            {false, ?error_msg}
    end.
            

%% 领取救济金
do_alms_reward(Role = #role{vip = Vip, coin = Coin}) ->
    #vip_welfare{alms = Alms} = vip:get_vip_welfare(Vip),
    Value = get_value(Role, ?daily_alms),
    case Value =:= 0 of
        true -> 
            case Coin >= Alms of
                true ->
                    {false, ?error_act};
                _ ->
                    {ok, NewRole} = do_add_coin(Role, Alms - Coin),
                    NewRole1 = add_value(NewRole, ?daily_alms),
                    {ok, coin, Alms - Coin, NewRole1}
            end;
        _ ->
            {false, ?error_num}
    end.

%% 登陆奖励抽奖
do_login_reward(Role = #role{vip = Vip}) ->
    Value = get_value(Role, ?daily_login),
    case Value =:= 0  of
        true -> 
            {Type, Num1, _} = login_reward(),
            Num2 = case Vip >= 1 of
                true -> 
                    Num1 * 2;
                _ ->
                    Num1
            end,
            {ok, NewRole} = do_add(Role, [{Type, Num2}]),
            NewRole1 = add_value(NewRole, ?daily_login),
            {ok, Type, Num2, NewRole1};
        _ ->
            {false, ?error_num}
    end.

%% Vip登陆奖励抽奖
do_vip_login_reward(Role = #role{vip = Vip}) when Vip > 0->
    case do_cost_coin(Role, 100000) of
        {ok, NewRole} ->
            {Type, Num1, _} = login_vip_reward(),
            {ok, NewRole1} = do_add(NewRole, [{Type, Num1}]),
            {ok, Type, Num1, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end;
do_vip_login_reward(_) -> {false, ?error_act}.


%% 随机奖励
login_reward() ->
    List = [{coin, 2000, 1000}, {coin, 8000, 1000}, {coin, 20000, 500}, {gold, 10, 1000}, {gold, 20, 500}, {ice, 1, 1000}, {locking, 1, 1000}, {tel_fare, 10, 100}],
    sys_rand:rand_list(List, 3).

%% 随机奖励
login_vip_reward() ->
    List = [{ice, 50, 1000}, {auto, 10, 1000}, {lollipop, 1, 1000}, {horn, 50, 500},  {gold, 200, 1000}, {gold_pick, 2, 1000}, {rage, 10, 100}, {self_horn, 5, 1000}, {locking, 100, 1000}, {coin, 200000, 1000}],
    sys_rand:rand_list(List, 3).


%% 获取人物每日数据,不带默认值，返回0
get_value(_Role = #role{daily_value = List}, Type) ->
    case lists:keyfind(Type, 1, List) of
        {Type, Value} -> Value;
        _ -> 0
    end.

%% 获取人物每日数据,带默认值，返回默认值
get_value(_Role = #role{daily_value = List}, Type, Default) ->
    case lists:keyfind(Type, 1, List) of
        {Type, Value} -> Value;
        _ -> Default
    end.


%% 设置人物每日数据
set_value(Role = #role{daily_value = List}, Type, Value) ->
    NewList = lists:keystore(Type, 1, List, {Type, Value}),
    Role#role{daily_value = NewList}.

%% 增加人物每日数据 仅限数据为integer
add_value(Role, Type) ->
    add_value(Role, Type, 1).

%% 增加人物每日数据 仅限数据为integer
add_value(Role = #role{daily_value = List}, Type, Add) ->
    NewList = case lists:keyfind(Type, 1, List) of
        {Type, Value} -> 
            lists:keyreplace(Type, 1, List, {Type, Value + Add});
        _ ->
            [{Type, Add} | List]
    end,
    Role#role{daily_value = NewList}.


%% 注意 必须要在人物进程调用，并且是begin 后面必须要flush或者clean
%% 消息缓冲推送
send_buff_begin() ->
    case get(send_buff) of
        undefined  ->
            put(send_buff, []);
        _ ->
            ?ERR("缓冲池已经开始了", []),
            ok
    end.

send_buff_flush() ->
    case get(socket_pid) of
        Pid when is_pid(Pid) ->
            case get(send_buff) of
                List when is_list(List)->
                    [Pid ! {tcp_send, Data} || Data<-List],
                    put(send_buff, undefined);
                _ ->
                    ?ERR("缓冲池没有数据", []),
                    ok
            end;
        _ ->
            ?ERR("不在人物进程不能调用", [])
    end.

send_buff_clean() ->
    put(send_buff, undefined).


%% 获取背包信息
get_bag_info(#role{item = #role_item{ice = Ice, horn = Horn, rage = Rage, trumpet = Trumpet, locking = Locking, auto = Auto, lollipop = Lollipop, tel_fare = Tel, red_bag = RedBag}, candy = Candy, lolly = Lolly, self_horn = SelfHorn, xin_card = Xin, nian_card = Nian, kuai_card = Kuai, le_card = Le, gold_pick = Pick, active_card = Card}) ->
    [#p_assets{type = ice, num = Ice}, #p_assets{type = horn, num = Horn}, #p_assets{type = rage, num = Rage}, #p_assets{type = trumpet, num = Trumpet}
        ,#p_assets{type = locking, num = Locking}, #p_assets{type = auto, num = Auto}, #p_assets{type = lollipop, num = Lollipop}, #p_assets{type = tel_fare, num = Tel}
        ,#p_assets{type = red_bag, num = RedBag}, #p_assets{type = candy, num = Candy},#p_assets{type = lolly, num = Lolly}, #p_assets{type = self_horn, num = SelfHorn}
        ,#p_assets{type = xin_card, num = Xin}, #p_assets{type = nian_card, num = Nian},#p_assets{type = kuai_card, num = Kuai}, #p_assets{type = le_card, num = Le}
        ,#p_assets{type = gold_pick, num = Pick}, #p_assets{type = active_card, num = Card}
    ].

%% 使用道具
use_item(Role, lollipop, Num) ->
    case do_cost_lollipop(Role, Num) of
        {ok, NewRole} ->
            {ok, NewRole1} = do_add_coin(NewRole, 2000000 * Num),
            {ok, 200000 * Num, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end;
use_item(Role, candy, Num) ->
    case do_cost_lollipop(Role, Num) of
        {ok, NewRole} ->
            {ok, NewRole1} = do_add_coin(NewRole, 40000 * Num),
            {ok, 4000 * Num, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end;
use_item(Role, lolly, Num) ->
    case do_cost_lollipop(Role, Num) of
        {ok, NewRole} ->
            {ok, NewRole1} = do_add_coin(NewRole, 400000 * Num),
            {ok, 40000 * Num, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end;
use_item(_, _, _) -> {false, ?error_item}.


%% 赠送道具记录
do_send_log(Role = #role{send_log = LogList}, Log) ->
    Role#role{send_log = lists:sublist([Log | LogList], 10)}.

%% 使用道具记录
do_use_log(Role = #role{use_log = LogList}, Log) ->
    Role#role{use_log = lists:sublist([Log | LogList], 10)}.

%% 话费兑换日志
do_exchange_log(Role = #role{exchange_log = LogList}, Log) ->
    Role#role{exchange_log = lists:sublist([Log | LogList], 10)}.


%% 使用喇叭
use_trumpet(Role = #role{name = Name}, Msg)->
    case check_msg(Msg) of
        true ->
            case do_cost(Role, [{trumpet, 1}]) of
                {ok, NewRole} ->
                    boradcast_mgr:trumpet(Name, Msg),
                    {ok, NewRole};
                _ ->
                    case do_cost_gold(Role, 20) of
                        {ok, NewRole} ->
                            boradcast_mgr:trumpet(Name, Msg),
                            {ok, NewRole};
                        {false, Reason} ->
                            {false, Reason}
                    end
            end;
        _ ->
            {false, ?error_msg}
    end.

%% 检查消息合法 暂时不处理
check_msg(_) -> true.




%% 扣除指定资产
%% 金币
do_cost_coin(Role = #role{coin = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = coin, num = Value - Cost}]}),
    {ok, Role#role{coin = Value - Cost}};
do_cost_coin(_, _) ->
    {false, ?error_coin}.
%% 钻石
do_cost_gold(Role = #role{gold = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value - Cost}]}),
    {ok, Role#role{gold = Value - Cost}};
do_cost_gold(_, _) ->
    {false, ?error_gold}.
%% 棒棒糖
do_cost_lollipop(Role = #role{item = Item = #role_item{lollipop = Value}}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = lollipop, num = Value - Cost}]}),
    {ok, Role#role{item = Item#role_item{lollipop = Value - Cost}}};
do_cost_lollipop(_, _) ->
    {false, ?error_item_num}.


%% 批量扣除资产
do_cost(Role, []) -> {ok, Role};
do_cost(Role, List) -> 
    do_cost(Role, List, []).

do_cost(Role, [], []) -> {ok, Role};
do_cost(Role, [], List) -> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = Type, num = Value}||{Type, Value} <-List]}),
    {ok, Role};
%% 金币
do_cost(Role = #role{coin = OldValue}, [{coin, Value} | L], List) when OldValue >= Value->
    do_cost(Role#role{coin = OldValue - Value}, L, [{coin, OldValue - Value} | List]);
do_cost(_Role, [{coin, _Value} | _L], _)->
    {false, ?error_coin};
%% 钻石
do_cost(Role = #role{gold = OldValue}, [{gold, Value} | L], List) when OldValue >= Value->
    do_cost(Role#role{gold = OldValue - Value}, L, [{gold, OldValue - Value} | List]);
do_cost(_Role, [{gold, _Value} | _L], _)->
    {false, ?error_gold};
%% 冰冻
do_cost(Role = #role{item = Item = #role_item{ice = OldValue}}, [{ice, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{ice = OldValue - Value}}, L, [{ice, OldValue - Value} | List]);
%% 号角
do_cost(Role = #role{item = Item = #role_item{horn = OldValue}}, [{horn, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{horn = OldValue - Value}}, L, [{horn, OldValue - Value} | List]);
%% 私有号角
do_cost(Role = #role{self_horn = OldValue}, [{self_horn, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{self_horn = OldValue - Value}, L, [{self_horn, OldValue - Value} | List]);
%% 金猪
do_cost(Role = #role{gold_pick = OldValue}, [{gold_pick, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{gold_pick = OldValue - Value}, L, [{gold_pick, OldValue - Value} | List]);
%% 女神卡
do_cost(Role = #role{active_card = OldValue}, [{active_card, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{active_card = OldValue - Value}, L, [{active_card, OldValue - Value} | List]);
%% 狂暴
do_cost(Role = #role{item = Item = #role_item{rage = OldValue}}, [{rage, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{rage = OldValue - Value}}, L, [{rage, OldValue - Value} | List]);
%% 喇叭
do_cost(Role = #role{item = Item = #role_item{trumpet = OldValue}}, [{trumpet, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{trumpet = OldValue - Value}}, L, [{trumpet, OldValue - Value} | List]);
%% 锁定
do_cost(Role = #role{item = Item = #role_item{locking = OldValue}}, [{locking, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{locking = OldValue - Value}}, L, [{locking, OldValue - Value} | List]);
%% 自动
do_cost(Role = #role{item = Item = #role_item{auto = OldValue}}, [{auto, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{auto = OldValue - Value}}, L, [{auto, OldValue - Value} | List]);
%% 棒棒糖
do_cost(Role = #role{item = Item = #role_item{lollipop = OldValue}}, [{lollipop, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{lollipop = OldValue - Value}}, L, [{lollipop, OldValue - Value} | List]);
%% 话费
do_cost(Role = #role{item = Item = #role_item{tel_fare = OldValue}}, [{tel_fare, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{tel_fare = OldValue - Value}}, L, [{tel_fare, OldValue - Value} | List]);
%% 红包
do_cost(Role = #role{item = Item = #role_item{red_bag = OldValue}}, [{red_bag, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{item = Item#role_item{red_bag = OldValue - Value}}, L, [{red_bag, OldValue - Value} | List]);

%% 小棒棒糖
do_cost(Role = #role{candy = OldValue}, [{candy, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{candy = OldValue - Value}, L, [{candy, OldValue - Value} | List]);
%% 中棒棒糖
do_cost(Role = #role{lolly = OldValue}, [{lolly, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{lolly = OldValue - Value}, L, [{lolly, OldValue - Value} | List]);

%% 新
do_cost(Role = #role{xin_card = OldValue}, [{xin_card, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{xin_card = OldValue - Value}, L, [{xin_card, OldValue - Value} | List]);
%% 年
do_cost(Role = #role{nian_card = OldValue}, [{nian_card, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{nian_card = OldValue - Value}, L, [{nian_card, OldValue - Value} | List]);
%% 快
do_cost(Role = #role{kuai_card = OldValue}, [{kuai_card, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{kuai_card = OldValue - Value}, L, [{kuai_card, OldValue - Value} | List]);
%% 乐
do_cost(Role = #role{le_card = OldValue}, [{le_card, Value} | L], List) when OldValue >= Value-> 
    do_cost(Role#role{le_card = OldValue - Value}, L, [{le_card, OldValue - Value} | List]);

do_cost(_Role, [{_Type, _} | _L], _List) ->
    {false, ?error_item_num}.



%% 增加指定资产
%% 金币
do_add_coin(Role = #role{coin = Value}, Add) ->
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = coin, num = Value + Add}]}),
    {ok, Role#role{coin = min(?int_max_num, Value + Add)}}.
%% 钻石
do_add_gold(Role = #role{gold = Value}, Add) ->
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value + Add}]}),
    {ok, Role#role{gold = Value + Add}}.


%% 批量增加资产
do_add(Role, []) -> {ok, Role};
do_add(Role, List) ->
    do_add(Role, List, []).

do_add(Role, [], []) ->  {ok, Role};
do_add(Role, [], List) -> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = Type, num = Value}||{Type, Value} <-List]}),
    {ok, Role};

%% 金币
do_add(Role = #role{coin = OldValue}, [{coin, Value} | L], List) -> 
    do_add(Role#role{coin = min(?int_max_num, OldValue + Value)}, L, [{coin, OldValue + Value} | List]);
%% 钻石
do_add(Role = #role{gold = OldValue}, [{gold, Value} | L], List) -> 
    do_add(Role#role{gold = OldValue + Value}, L, [{gold, OldValue + Value} | List]);

%% 冰冻
do_add(Role = #role{item = Item = #role_item{ice = OldValue}}, [{ice, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{ice = min(99, OldValue + Value)}}, L, [{ice, OldValue + Value} | List]);
%% 号角
do_add(Role = #role{item = Item = #role_item{horn = OldValue}}, [{horn, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{horn = min(99, OldValue + Value)}}, L, [{horn, OldValue + Value} | List]);
%% 私有号角
do_add(Role = #role{self_horn = OldValue}, [{self_horn, Value} | L], List) -> 
    do_add(Role#role{self_horn = min(99, OldValue + Value)}, L, [{self_horn, OldValue + Value} | List]);
%% 金猪
do_add(Role = #role{gold_pick = OldValue}, [{gold_pick, Value} | L], List) -> 
    do_add(Role#role{gold_pick = min(99, OldValue + Value)}, L, [{gold_pick, OldValue + Value} | List]);
%% 女神卡
do_add(Role = #role{active_card = OldValue}, [{active_card, Value} | L], List) -> 
    do_add(Role#role{active_card = OldValue + Value}, L, [{active_card, OldValue + Value} | List]);
%% 狂暴
do_add(Role = #role{item = Item = #role_item{rage = OldValue}}, [{rage, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{rage = min(99, OldValue + Value)}}, L, [{rage, OldValue + Value} | List]);
%% 喇叭
do_add(Role = #role{item = Item = #role_item{trumpet = OldValue}}, [{trumpet, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{trumpet = min(99, OldValue + Value)}}, L, [{trumpet, OldValue + Value} | List]);
%% 锁定
do_add(Role = #role{item = Item = #role_item{locking = OldValue}}, [{locking, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{locking = min(99, OldValue + Value)}}, L, [{locking, OldValue + Value} | List]);
%% 自动
do_add(Role = #role{item = Item = #role_item{auto = OldValue}}, [{auto, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{auto = min(99, OldValue + Value)}}, L, [{auto, OldValue + Value} | List]);
%% 棒棒糖
do_add(Role = #role{item = Item = #role_item{lollipop = OldValue}}, [{lollipop, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{lollipop = OldValue + Value}}, L, [{lollipop, OldValue + Value} | List]);
%% 话费
do_add(Role = #role{item = Item = #role_item{tel_fare = OldValue}}, [{tel_fare, Value} | L], List) -> 
    do_add(Role#role{item = Item#role_item{tel_fare = OldValue + Value}}, L, [{tel_fare, OldValue + Value} | List]);
%% 红包
do_add(Role = #role{item = Item = #role_item{red_bag = OldValue}}, [{red_bag, Value} | L], List) -> 
    NewRole  = do_set_sheep(Role#role{item = Item#role_item{red_bag = OldValue + Value}}),
    NewRole1 = task:handle_guide(NewRole, red_bag, Value),
    do_add(NewRole1, L, [{red_bag, OldValue + Value} | List]);

%% 小棒棒糖
do_add(Role = #role{candy = OldValue}, [{candy, Value} | L], List) -> 
    do_add(Role#role{candy = OldValue + Value}, L, [{candy, OldValue + Value} | List]);
%% 中棒棒糖
do_add(Role = #role{lolly = OldValue}, [{lolly, Value} | L], List) -> 
    do_add(Role#role{lolly = OldValue + Value}, L, [{lolly, OldValue + Value} | List]);

%% 新
do_add(Role = #role{xin_card = OldValue}, [{xin_card, Value} | L], List) -> 
    do_add(Role#role{xin_card = OldValue + Value}, L, [{xin_card, OldValue + Value} | List]);
%% 年
do_add(Role = #role{nian_card = OldValue}, [{nian_card, Value} | L], List) -> 
    do_add(Role#role{nian_card = OldValue + Value}, L, [{nian_card, OldValue + Value} | List]);
%% 快
do_add(Role = #role{kuai_card = OldValue}, [{kuai_card, Value} | L], List) -> 
    do_add(Role#role{kuai_card = OldValue + Value}, L, [{kuai_card, OldValue + Value} | List]);
%% 乐
do_add(Role = #role{le_card = OldValue}, [{le_card, Value} | L], List) -> 
    do_add(Role#role{le_card = OldValue + Value}, L, [{le_card, OldValue + Value} | List]);

do_add(Role, [{Type, _} | L], List) ->
    ?ERR("增加未知的资产类型:~w", [Type]),
    do_add(Role, L, List).





%% 身份证验证
check_id_card(Id) ->
    case erlang:length(Id) of
        18 ->
            Year = lists:sublist(Id, 7, 4),
            Month = lists:sublist(Id, 11, 2),
            Day = lists:sublist(Id, 13, 2),
            case check([{date, {Year, Month, Day}}]) of
                true ->
                    Last = do_last(lists:sublist(Id, 17), ?value_list, 0),
                    [lists:last(Id)] =:= Last;
                _ ->
                    false
            end;
        15 ->
            First = lists:sublist(Id, 1, 6),
            Mind = lists:sublist(Id, 7, 2),
            Last = lists:sublist(Id, 9, 7),
            All = First ++ "19" ++ Mind ++ Last,
            Last1 = do_last(All, ?value_list, 0),
            check(All ++ Last1);
        _ ->
            false
    end.

check([]) -> true;
check([{date, {Year, Month, Day}} | L]) -> 
    Year1 = list_to_integer(Year),
    Month1 = list_to_integer(Month),
    Day1 = list_to_integer(Day),
    case catch date:datetime_to_seconds({Year1, Month1, Day1, 0, 0, 0}) of
        Date when is_integer(Date) ->
            check(L);
        _ -> false
    end.

do_last("", [], Num) -> 
    get_rem(Num rem 11);
do_last([N | L], [N1 | L1], Num) -> 
    Add = list_to_integer([N]) * N1,
    do_last(L, L1, Num + Add).

get_rem(0) -> "1";
get_rem(1) -> "0";
get_rem(2) -> "X";
get_rem(3) -> "9";
get_rem(4) -> "8";
get_rem(5) -> "7";
get_rem(6) -> "6";
get_rem(7) -> "5";
get_rem(8) -> "4";
get_rem(9) -> "3";
get_rem(10) -> "1".


%% 领取红包
reward_red_bag(_Role = #role{red_openid = ""}) ->
    {false, ?error_red_bag_code};
reward_red_bag(Role = #role{vip = Vip, red_openid = OpenId, role_id = RoleID, item = #role_item{red_bag = RedBag}, exchange = Exchange, channel = Channel}) ->
    Need = case setting_mgr:get(?setting_redbag) of
        {ok, Setting} -> Setting * 100;
        _ -> ?redbag_setting * 100
    end,
    case RedBag >= Need of
        true ->
            N = get_value(Role, ?daily_red_bag),
            #vip_welfare{max_red = Max} = vip:get_vip_welfare(Vip),
            case N >= Max of
                true ->
                    {false, ?error_red_bag_num};
                _ ->
                    Value = min(500000, (RedBag div Need)),  %% 分
                    Flow = functions_mgr:get_exchange_flow(), %% 元
                    case Flow * 100 >= Value of
                        true ->
                            Now = date:unixtime(),
                            ParTraNo = lists:concat([sys_env:get_env(redMachId), Now, sys_rand:rand(1000,9999)]),
                            case db:exec("insert into red_packet_log(role_id, cny, order_id, time) value(?, ?, ?, ?)", [RoleID, Value, ParTraNo, Now]) of
                                ok -> 
                                    role_lib:send_buff_begin(),
                                    {ok, NewRole} = do_cost(Role, [{red_bag, Value}]),  
                                    case send_red_bag(ParTraNo, OpenId, Value) of
                                        {ok, ParTraNo} ->
                                            db:exec("update red_packet_log set status = ? where order_id = ?", [1, ParTraNo]),
                                            NewRole1 = add_value(NewRole, ?daily_red_bag),
                                            case Channel of
                                                0 -> ok;
                                                _ ->
                                                    db:exec("insert into channel_exchange_log(channel_id, role_id, exchange, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleID, Value, 3, date:unixtime()])
                                            end,
                                            functions_mgr:delete_exchange_flow(trunc(Value/100)),
                                            role_lib:send_buff_flush(),
                                            {ok, NewRole1#role{exchange = Exchange + Value}};
                                        {error, _Reason = 49}->
                                            ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                                            role_lib:send_buff_clean(),
                                            {false, ?error_red_bag_num_limit};
                                        {error, _Reason = 46}->
                                            ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                                            role_lib:send_buff_clean(),
                                            {false, ?error_red_bag_not_exist};
                                        {error, _Reason = 50}->
                                            ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                                            role_lib:send_buff_clean(),
                                            {false, ?error_red_bag_account};
                                        {error, _Reason} when is_integer(_Reason)->
                                            ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                                            role_lib:send_buff_clean(),
                                            {false, ?error_busy};
                                        _ ->
                                            case check_red_bag(ParTraNo)of
                                                ok ->
                                                    db:exec("update red_packet_log set status = ? where order_id = ?", [1, ParTraNo]),
                                                    {ok, NewRole} = do_cost(Role, [{red_bag, Value}]),  
                                                    NewRole1 = add_value(NewRole, ?daily_red_bag),
                                                    case Channel of
                                                        0 -> ok;
                                                        _ ->
                                                            db:exec("insert into channel_exchange_log(channel_id, role_id, exchange, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleID, Value, 3, date:unixtime()])
                                                    end,
                                                    functions_mgr:delete_exchange_flow(trunc(Value/100)),
                                                    role_lib:send_buff_flush(),
                                                    {ok, NewRole1#role{exchange = Exchange + Value}};
                                                _ ->
                                                    ?ERR("企业付款超时失败~w, ~ts, ~w", [RoleID, ParTraNo, Value]),
                                                    role_lib:send_buff_clean(),
                                                    {false, ?error_busy}
                                            end
                                    end;
                                _ ->
                                    {false, ?error_busy}
                            end;
                        _ ->
                            {false, ?error_exchange_flow}
                    end
            end;
        _ ->
            {false, ?error_red_bag_min}
    end.

check_red_bag(OrderID) ->
    Url = "https://api.mch.weixin.qq.com/mmpaymkttransfers/gettransferinfo",
    MchID = sys_env:get_env(redMachId),
    AppID = sys_env:get_env(redAppId),
    Key = sys_env:get_env(redMachKey),
    Desc = erlang:binary_to_list(unicode:characters_to_binary("是否到账")),
    SS = [
        {appid, AppID}
        ,{mch_id, MchID}
        ,{nonce_str, Desc}
        ,{partner_trade_no, OrderID}
        ,{key, Key}
    ],
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    HttpOps = [{ssl, [{certfile, "pem/apiclient_cert.pem"}, {keyfile, "pem/apiclient_key.pem"}]}],
    case httpc:request(post, {Url, Headers, ContentType, Xml}, HttpOps, []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "/xml/result_code") of
                {ok, "SUCCESS"} ->
                    case util:get_xml_info(Result, "/xml/status") of
                        {ok, "SUCCESS"} ->
                            ok;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.



%% 公众号发送红包
send_red_bag(ParTraNo, OpenID, Amount) ->
    MchID = sys_env:get_env(redMachId),
    Url = "https://api.mch.weixin.qq.com/mmpaymkttransfers/promotion/transfers",
    IP = sys_env:get_env(ip),
    AppID = sys_env:get_env(redAppId),
    Key = sys_env:get_env(redMachKey),
    NonceStr = string:to_upper(erlang:binary_to_list(util:md5(ParTraNo))),
    Desc = erlang:binary_to_list(unicode:characters_to_binary("感谢您参加，祝您生活愉快")),
    SS = [
        {amount, Amount},
        {check_name, 'NO_CHECK'},
        {desc, Desc},
        {mch_appid, AppID},
        {mchid, MchID},
        {nonce_str, NonceStr},
        {openid, OpenID},
        {partner_trade_no, ParTraNo},
        {spbill_create_ip, IP},
        {key, Key}
    ],
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    HttpOps = [{ssl, [{certfile, "pem/apiclient_cert.pem"}, {keyfile, "pem/apiclient_key.pem"}]}],
    case httpc:request(post, {Url, Headers, ContentType, Xml}, HttpOps, []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "/xml/result_code") of
                {ok, "SUCCESS"} ->
                    case util:get_xml_info(Result, "/xml/partner_trade_no") of
                        {ok, ParTraNo} ->
                            {ok, ParTraNo};
                        _R ->
                            {ok, ParTraNo}
                    end;
                {ok, "FAIL"} ->
                    EC = util:get_xml_info(Result, "/xml/err_code"),
                    case EC of
                        {_, "V2_ACCOUNT_SIMPLE_BAN"} ->
                            {error, 50};
                        {_, "AMOUNT_LIMIT"} ->
                            {error, 53};
                        {_, "NOTENOUGH"} ->
                            {error, 46};
                        {_, "PAYMENT_ACCOUNT_NOT_EXIST"} ->
                            {error, 46};
                        {_, "SENDNUM_LIMIT"} ->
                            {error, 49};
                        {_, "SIGN_ERROR"} ->
                            {error, 73};
                        {_, "CA_ERROR"} ->
                            {error, 52};
                        {_, "REQ_PARAM_XML_ERR"} ->
                            {error, 74};
                        {_, "COUPON_STOCK_ID_EMPTY"} ->
                            {error, 75};
                        {_, "MCH_ID_EMPTY"} ->
                            {error, 76};
                        {_, "CODE_2_ID_ERR"} ->
                            {error, 77};
                        {_, "OPEN_ID_EMPTY"} ->
                            {error, 78};
                        {_, "ERR_VERIFY_SSL_SERIAL"} ->
                            {error, 79};
                        {_, "ERR_VERIFY_SSL_SN"} ->
                            {error, 80};
                        {_, "CA_VERIFY_FAILED"} ->
                            {error, 52};
                        {_, "NO_AUTH"} ->
                            case util:get_xml_info(Result, "/xml/err_code_des") of
                                {ok, [20135,21697,26435,38480,39564,35777,22833,36133,44,
                                        35831,26597,30475,24744,24403,21069,26159,21542,20855,26377,
                                        35813,20135,21697,30340,26435,38480]} ->
                                    {error, 63};
                                _ ->
                                    {error, 50}
                            end;
                        {_, "ILLEGAL_APPID"} ->
                            {error, 81};
                        {_, "MONEY_LIMIT"} ->
                            {error, 53};
                        {_, "SEND_FAILED"} ->
                            {error, 82};
                        {_, "FATAL_ERROR"} ->
                            {error, 83};
                        {_, "OPENID_ERROR"} ->
                            {error, 48};
                        {_, "SYSTEMERROR"} ->
                            {error, 1};
                        {_, _ErrCode} ->
                            case util:get_xml_info(Result, "/xml/err_code_des") of
                                {_, Des} ->
                                    case Des of
                                        [21442,25968,38169,35823,58,36755,20837,30340,29992,25143,111,112,101,110,105,100,26377,35823,46] ->
                                            {error, 48};
                                        [21442,25968,38169,35823,65306,29992,
                                            25143,111,112,101,110,105,100,23383,
                                            27573,24517,22635,65292,24182,19988,
                                            23569,20110,51,50,20010,23383,31526,
                                            46] ->
                                            {error, 48};
                                        [38750,23454,21517,29992,25143,
                                            36134,21495,19981,21487,21457,
                                            25918] ->
                                            {error, 50};
                                        [31995,32479,32321,24537,65292,35831,
                                            31245,21518,20877,35797] ->
                                            {error, 65};
                                        _ ->
                                            ?ERR("提现失败错误~w", [_ErrCode]),
                                            {error, 47}
                                    end;
                                _ ->
                                    ?ERR("提现失败错误~w", [_ErrCode]),
                                    {error, 47}
                            end;
                        _R ->
                            ?ERR("提现失败错误~w", [_R]),
                            {error, 47}
                    end;
                _ ->
                    {error, 1}
            end
    end.


