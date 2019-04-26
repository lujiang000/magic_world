%%----------------------------------------------------
%% @doc 执行脚本
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(script).
-export([
        do_all_channel/0
        ,do_friend/0
        ,do_test/1
        ,do_sort/1
        ,do_rank/0
        ,set_role/3
        ,apply_set_role/3
    ]
).

-include("common.hrl").
-include("role.hrl").
-include("rank.hrl").

-record(friend, {
        role_id = 0
        ,list = []
        ,coin = 0
        ,all_coin = 0
        ,num = 0
        ,all_num = 0
    }
).

%% 合作商的处理
do_all_channel() ->
    case db:get_all("select id from channel") of
        {ok, List} ->
            do_all_channel(List);
        _ ->
            ?ERR("没有合作商")
    end.

do_all_channel([]) -> ok;
do_all_channel([[Id] | L]) ->
    case db:get_all("select role_id from role where channel = ?", [Id]) of
        {ok, List} ->
            do_all_channel_1(Id, List);
        _ ->
            ok
    end,
    do_all_channel(L).

do_all_channel_1(_, []) -> ok;
do_all_channel_1(Id, [[RoleId] | L]) ->
    do_channel_charge(Id, RoleId),
    do_channel_exchange(Id, RoleId),
    do_all_channel_1(Id, L).


do_channel_charge(Id, RoleId) ->
    case db:get_all("select role_id, charge_rmb, type, call_time from charge_log where role_id = ? and status = 1", [RoleId]) of
        {ok, List} ->
            do_channel_charge_1(Id, List);
        _ ->
            ok
    end.

do_channel_charge_1(Id, List) ->
    [db:exec("insert into channel_charge_log(channel_id, role_id, charge_rmb, type, time) value (?, ?, ?, ?, ?)", [Id, RoleId, Rmb, Type, Time])||[RoleId, Rmb, Type, Time] <-List].

do_channel_exchange(Id, RoleId) ->
    List4 = case db:get_all("select cny, time from red_packet_log where role_id = ?", [RoleId]) of
        {ok, List1} ->
            [{Rmb, Time, 3}||[Rmb, Time] <-List1];
        _ ->
            []
    end,
    List5 = case db:get_all("select role_id, price, time  from jd_card where role_id = ?", [RoleId]) of
        {ok, List2} ->
            [{Rmb * 100, Time, 2}||[Rmb, Time] <-List2];
        _ ->
            []
    end,
    List6 = case db:get_all("select role_id, price, time from phone_card where role_id = ?", [RoleId]) of
        {ok, List3} ->
            [{Rmb * 100, Time, 1}||[Rmb, Time] <-List3];
        _ ->
            []
    end,
    List = List4 ++ List5 ++ List6,
    [db:exec("insert into channel_exchange_log(channel_id, role_id, exchange, type, time) value (?, ?, ?, ?, ?)", [Id, RoleId, Rmb, Type, Time])||{Rmb, Time, Type} <-List].



do_friend() ->
    List = ets:tab2list(friend),
    do_friend(List).

do_friend([]) ->ok;
do_friend([Friend = #friend{list = List, coin = Coin, all_coin = AllCoin} | L]) ->
    Charge = do_friend_charge(List, 0),
    Add = trunc(Charge * 100 * 5 /100),
    ets:insert(friend, Friend#friend{coin = (Coin - Add) * 10 + Add, all_coin = (AllCoin - Add) * 10 + Add}),
    do_friend(L).


do_friend_charge([], AllNum) -> AllNum;
do_friend_charge([RoleId | L], AllNum) ->
    Time = 1539936000,
    N = case db:get_one("select sum(charge_rmb) from charge_log where role_id = ? and status = 1 and time > ?", [RoleId, Time]) of
        {ok, Num} when is_integer(Num) ->
            Num;
        _ ->
            0
    end,
    do_friend_charge(L, AllNum + N).

do_test(N) ->
    do_test(N, 20000, 0, []).

do_test(0, _Coin, All, List) -> {All, List};
do_test(N, Coin, All, List) ->
    List1 = [{1, 544}, {6, 5000}, {8, 8000}, {10, 4000}, {12, 8000}, {20, 2000}, {5000, 2}],
    {Num, _} = sys_rand:rand_list(List1, 2),
    Add = erlang:trunc(Coin * Num/100),
    NewList = case lists:keyfind(Num, 1, List) of
        {Num, Num1} ->
            lists:keyreplace(Num, 1, List, {Num, Num1 + 1});
        _ ->
            [{Num, 1} | List]
    end,
    do_test(N - 1, Coin, All + Add, NewList).

do_sort(List) ->
    do_sort(List, []).

do_sort([], List) -> List;
do_sort(List, L) -> 
    {Max, L1} = get_max(List),
    do_sort(L1, [Max | L]).

get_max([A]) -> {A, []};
get_max([A | L]) -> 
    get_max(A, L, []).

get_max(A, [], List) -> {A, List};
get_max(A, [B | L], List) when A >= B -> 
    get_max(A, L, [B | List]);
get_max(A, [B | L], List) -> 
    get_max(B, L, [A | List]).



do_rank() ->
    case db:get_all("select role_id, sum(cost_value) as all_num from coin_cost_log where id > 327639641 and cost_value > 0 and type = 1 group by role_id order by all_num desc limit 0, 100") of
        {ok, List} ->
            do_rank(List);
        _ ->
            false
    end.

do_rank([]) -> ok;
do_rank([[RoleID, Value] | L]) ->
    {ok, Role} = role_data:get_role_from_dets(RoleID),
    rank:handle(?rank_kill, Role#role{daily_kill = Value}),
    do_rank(L).


set_role(RoleId, Type, Value) ->
  case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, apply_set_role, [Type, Value]});
        _ ->
            case role_data:get_role_from_dets(RoleId) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_set_role(Role1, Type, Value),
                    role_data:save(NewRole);
                _ ->
                    false
            end
    end.

apply_set_role(Role, icon, Value) ->
    {ok, Role#role{icon = Value}};
apply_set_role(Role = #role{item = Item = #role_item{tel_fare = _Tel}}, tel_fare, Value) ->
    {ok, Role#role{item = Item#role_item{tel_fare = Value}}};
apply_set_role(Role, vip_charge, Value) ->
    NewVip = vip:get_lev(Value),
    {ok, Role#role{vip = NewVip, vip_charge = Value}};
apply_set_role(Role, open_id, Value) ->
    {ok, Role#role{open_id = Value}};
apply_set_role(Role, phone, Value) ->
    {ok, Role#role{phone = Value}};
apply_set_role(Role, name, Value) ->
    {ok, Role#role{name = Value}};
apply_set_role(Role = #role{item = Item}, tel_fare, Value) ->
    {ok, Role#role{item = Item#role_item{tel_fare = Value}}};
apply_set_role(Role, _, _Value) ->
    {ok, Role}.


