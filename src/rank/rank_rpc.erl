%%----------------------------------------------------
%% @doc 排行榜协议处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(rank_rpc).
-export([
        handle/3

    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("rank.hrl").
-include("error_msg.hrl").


handle(1201, #m_1201_tos{start = Start, type = Type, num = Num, up_or_down = UpDown, date = Date}, Role = #role{item = Item}) ->
    {List, N, Value} = 
    case Date =:= 0 orelse Date =:= undefined of
        true -> rank_mgr:get_rank_info(Role, Start, Type, Num, UpDown);
        _ -> rank_mgr:get_old_rank_info(Role, Start, Type, Num, UpDown, Date)
    end,
    Value1 = case N of
        0 ->
            case Type of
                ?rank_coin -> 
                    Role#role.coin;
                ?rank_lollipop ->
                    Item#role_item.lollipop;
                ?rank_profit ->
                    Role#role.profit;
                ?rank_kill ->
                    Role#role.daily_kill;
                ?rank_kill_week ->
                    Role#role.week_kill
            end;
        _ ->
            Value
    end,
    {reply, #m_1201_toc{list = List, num = N, value = Value1}};

handle(1202, #m_1202_tos{type = _Type}, _Role) ->
    {List1, Type} = case setting_mgr:get(?setting_kill_rank_on) of
        {ok, 1} ->
            case setting_mgr:get(?setting_kill_rank) of
                {ok, List} ->
                    {[#p_rank_reward{num = N, item_type = mail_mgr:to_atom_assets(ItemType), item_num = ItemNum} || [N, ItemType, ItemNum]<- List], ?rank_kill};
                _ ->
                    {[], ?rank_kill}
            end;
        _ ->
            case setting_mgr:get(?setting_kill_rank_week_on) of
                {ok, 1} -> 
                    case setting_mgr:get(?setting_kill_rank_week) of
                        {ok, List} ->
                            {[#p_rank_reward{num = N, item_type = mail_mgr:to_atom_assets(ItemType), item_num = ItemNum} || [N, ItemType, ItemNum]<- List], ?rank_kill_week};
                        _ ->
                            {[], ?rank_kill_week}
                    end;
                _ ->
                    {[], 0}
            end
    end,
    {reply, #m_1202_toc{list = List1, type = Type}};

handle(_Cmd, _Data, _Role) ->
    ?ERR("错误的协议数据cmd:~w,data:~w", [_Cmd, _Data]),
    ok.

