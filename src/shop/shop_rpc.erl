%%----------------------------------------------------
%% @doc 商店协议入口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(shop_rpc).
-export([handle/3]).

-include("common.hrl").
-include("all_pb.hrl").


%% 获取商店状态
handle(1601, _, _Role) ->
    {Start, End, Status,  Num, AllNum} = shop:get_shop_status(),
    {reply, #m_1601_toc{start_time = Start, end_time = End, status = Status, num = Num, all_num = AllNum}};

%% 获取商店物品
handle(1602, _, _Role) ->
    Items = shop:get_shop_items(),
    {reply, #m_1602_toc{list = Items}};

%% 兑换
handle(1605, #m_1605_tos{id = Id, phone = Phone}, Role) ->
    case shop:exchange(Role, Id, Phone) of
        {ok, NewRole} ->
            {ok, #m_1605_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

handle(_Cmd, _Data, _) ->
    ?ERR("错误的协议~w:~w", [_Cmd, _Data]),
    {error, 1}.
