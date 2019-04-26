%%----------------------------------------------------
%% 神秘商店
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(shop).
-behaviour(gen_server).
-export([start_link/0
        ,reload/0
        ,get_shop_status/0
        ,get_shop_items/0
        ,exchange/3
        ,reload/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        start_time = 0
        ,end_time = 0
        ,items = []
        ,status = 0    %% 0未开启，1开启
        ,num = 0
        ,all_num = 0
    }
).

-record(shop_item, {
        id = 0
        ,type = 0
        ,price = 0
        ,num = 1
        ,need_num = 1
    }
).

-define(shop_start_time, date:datetime_to_seconds({2018, 9, 15, 0, 0, 0})).
-define(shop_end_time, date:datetime_to_seconds({2018, 9, 16, 0, 0, 0})).
-define(shop_items, [
        #shop_item{id = 1, type = gold, price = 200, need_num = 1}
        ,#shop_item{id = 2, type = tel_fare, price = 100, need_num = 2}
        ,#shop_item{id = 3, type = jd_card, price = 200, need_num = 4}
        ,#shop_item{id = 4, type = jd_card, price = 500, need_num = 10}
    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

%% 重载商店
reload() ->
    ?MODULE ! reload.

reload(?setting_shop) ->
    ?MODULE ! reload;
reload(_) -> ok.


%% 获取商店状态
get_shop_status() ->
    case catch gen_server:call(?MODULE, status) of
        {ok, StartTime, EndTime, Status, Num, AllNum} -> {StartTime, EndTime, Status, Num, AllNum};
        _ -> {0, 0, 0, 0, 0}
    end.

%% 获取商店物品列表
get_shop_items() ->
    case catch gen_server:call(?MODULE, get_items) of
        {ok, Items} -> 
            to_p_shop_item(Items);
        _ -> []
    end.

to_p_shop_item(List) -> 
    [#p_shop_item{id = Id, type = Type, price = Price, need_num = Need}||#shop_item{id = Id, type = Type, price = Price, need_num = Need} <-List].

%% 兑换物品
exchange(Role = #role{role_id = RoleId}, Id, Phone) ->
    case catch gen_server:call(?MODULE, {exchange, Id}) of
        {ok, Type, Price, Need} ->
            case role_lib:do_cost_lollipop(Role, Need) of
                {ok, NewRole} ->
                    case do_exchange(NewRole, Type, Price, Phone, Need) of
                        {ok, NewRole1} -> 
                            log_db:log(shop_exchange_log, insert, [RoleId, 1, Need, type_to_integer(Type), Price, date:unixtime()]),
                            {ok, NewRole1};
                        {false, Reason} -> 
                            ?MODULE ! {add_num, Need},
                            {false, Reason}
                    end;
                {false, Reason} ->
                    ?MODULE ! {add_num, Need},
                    {false, Reason}
            end;
        {false, Reason} ->
            {false, Reason};
        _ ->
            {false, ?error_busy}
    end.

type_to_integer(jd_card) -> 1;
type_to_integer(tel_fare) -> 2;
type_to_integer(gold) -> 3;
type_to_integer(_) -> 0.

%% 进行兑换
do_exchange(Role, gold, Price, _, _) ->
    role_lib:do_add_gold(Role, Price * 10);
do_exchange(Role = #role{role_id = RoleId, exchange = Exchange, channel = Channel}, tel_fare, Price, Phone, Need) ->
    case lib_juhe:check_phone(Phone, Price) of
        true ->
            OrderID = lists:concat([tel, RoleId, date:unixtime()]),
            case lib_juhe:direct_recharge(Phone, Price, OrderID) of
                true ->
                    Now = date:unixtime(),
                    mail_mgr:send(0, RoleId, "兑换好礼", util:fbin("恭喜成功为手机号：~ts 充值~w元话费", [Phone, Price]), [], Now),
                    log_db:log(phone_card, insert, [RoleId, 3, Need, Phone, Price, Now]),
                    case Channel of
                        0 -> ok;
                        _ ->
                            db:exec("insert into channel_exchange_log(channel_id, role_id, exchange, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleId, Price * 100, 1, Now])
                    end,
                    {ok, Role#role{exchange = Exchange + Price * 100}};
                _ ->
                    {false, ?error_busy}
            end;
        _ ->
            {false, ?error_phone}
    end;
do_exchange(Role = #role{role_id = RoleId, exchange = Exchange, channel = Channel}, jd_card, Price, _, Need) ->
    Now = date:unixtime(),
    OrderID = lists:concat([jd, RoleId, Now]),
    case lib_juhe:jd_card(Price, OrderID) of
        {ok, Cami} ->
            mail_mgr:send(0, RoleId, "兑换好礼", util:fbin("恭喜成功兑换~w京东礼品卡:~n ~ts", [Price, Cami]), [], Now),
            log_db:log(jd_card, insert, [RoleId, 3, Need, Cami, Price, Now]),
            case Channel of
                0 -> ok;
                _ ->
                    db:exec("insert into channel_exchange_log(channel_id, role_id, exchange, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleId, Price * 100, 2, Now])
            end,
            {ok, Role#role{exchange = Exchange + Price * 100}};
        _R -> 
            {false, ?error_busy}
    end.


%%mail_mgr:send(0, 1014583, "兑换好礼", util:fbin("恭喜成功兑换~w京东礼品卡:~n ~ts", [50, "0461-57DE-246E-1405"]), [], 1543128740).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    {Start, End, Items, Num} = do_reload(),
    NewState = do_init(#state{start_time = Start, end_time = End, items = Items, num = Num, all_num = Num}),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, NewState}.

handle_call(status, _From, State = #state{start_time = Start, end_time = End, status = Status, num = Num, all_num = AllNum}) ->
    {reply, {ok, Start, End, Status, Num, AllNum}, State};

handle_call(get_items, _From, State = #state{items = Items, status = 1}) ->
    {reply, {ok, Items}, State};
handle_call(get_items, _From, State) ->
    {reply, {false, ?error_shop_status}, State};


handle_call({exchange, Id}, _From, State = #state{items = Items, status = 1, num = Num}) ->
    case lists:keyfind(Id, #shop_item.id, Items) of
        #shop_item{type = Type, price = Price, need_num = Need} when Num >= Need->
            {reply, {ok, Type, Price, Need}, State#state{num = Num - Need}};
        _ ->
            {reply, {false, ?error_shop_item}, State}
    end;
handle_call({exchange, _Id}, _From, State) ->
    {reply, {false, ?error_shop_status}, State};


handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload, State) ->
    {Start, End, Items, Num} = do_reload(),
    NewState = do_init(State#state{start_time = Start, end_time = End, items = Items, num = Num, all_num = Num}),
    {noreply, NewState};

handle_info(stop, State) ->
    push_shop_status(0),
    {noreply, State#state{status = 0}};
handle_info(start, State) ->
    push_shop_status(1),
    {noreply, State#state{status = 1}};

%% 聚合兑换失败返回数量
handle_info({add_num, Num}, State = #state{num = Num1}) ->
    {noreply, State#state{num = Num + Num1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 重新加载活动
do_reload() ->
    case setting_mgr:get(?setting_shop) of
        {ok, {Start, End, Num}} ->
            {Start, End, ?shop_items, Num};
        _ ->
            {?shop_start_time, ?shop_end_time, ?shop_items, 100}
   end.

%% 初始化
do_init(State = #state{start_time = Start, end_time = End}) ->
    Now = date:unixtime(),
    if Now < Start ->
            case get(start_time) of
                Ref1 when is_reference(Ref1)->
                    erlang:cancel_timer(Ref1);
                _ ->
                    ok
            end,
            Ref = erlang:send_after((Start - Now) * 1000, self(), start),
            put(start_time, Ref),
            State#state{status = 0};
        Now >= Start andalso Now =< End ->
            case get(end_time) of
                Ref1 when is_reference(Ref1)->
                    erlang:cancel_timer(Ref1);
                _ ->
                    ok
            end,
            Ref = erlang:send_after((End - Now) * 1000, self(), stop),
            put(end_time, Ref),
            State#state{status = 1};
        true ->
            State#state{status = 0}
    end.


%% 推送活动开始
push_shop_status(Status) ->
    Data = #m_1603_toc{status = Status},
    List = ets:tab2list(online_role),
    [sys_conn:pack_send(Pid, 1603, Data)||#online_role{socket_pid = Pid} <-List].



