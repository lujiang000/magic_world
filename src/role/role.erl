%%----------------------------------------------------
%% 人物进程相关处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role).
-behaviour(gen_server).
-export([start/1
        ,apply/3
        ,handle_rpc/5
        ,do_zero_flush/1
        ,do_change/3
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("error_msg.hrl").
-include("all_pb.hrl").
-include("rank.hrl").

%% 创建角色
start(Role = #role{}) ->
    gen_server:start(?MODULE, [Role], []).

apply(async, RolePid, {F}) ->
    RolePid ! {apply_async, {F}},
    ok;
apply(async, RolePid, {F, A}) ->
    RolePid ! {apply_async, {F, A}},
    ok;
apply(async, RolePid, {M, F, A}) ->
    RolePid ! {apply_async, {M, F, A}},
    ok;
apply(sync, RolePid, Mfa) when self() =:= RolePid ->
    {M, F, _, _} = hd(tl(util:get_stacktrace())),
    ?ERR("调用者[~w:~w]执行apply[~w]时发生错误：调用了自身", [M, F, Mfa]),
    {error, self_call};
apply(sync, RolePid, {F}) ->
    gen_server:call(RolePid, {apply_sync, F});
apply(sync, RolePid, {F, A}) ->
    gen_server:call(RolePid, {apply_sync, {F, A}});
apply(sync, RolePid, {M, F, A}) ->
    gen_server:call(RolePid, {apply_sync, {M, F, A}}).


%% 玩家协议调用
handle_rpc(Pid, Mod, Cmd, Data, Flag) ->
    catch Pid ! {rpc, Mod, Cmd, Data, Flag}.



init([Role = #role{socket_pid = SocketPid, role_id = RoleID}]) ->
    process_flag(trap_exit, true),
    put(socket_pid, SocketPid),
    link(SocketPid),
    SocketPid ! {save_role_pid, self(), RoleID},
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), zero_flush),
    NewRole = role_login:do(Role#role{pid = self()}),
    self() ! init,
    {ok, NewRole}.

%% 执行同步apply操作
handle_call({apply_sync, {F}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(F, [Role]), {undefined, F, []}, Role);
handle_call({apply_sync, {F, A}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(F, [Role | A]), {undefined, F, A}, Role);
handle_call({apply_sync, {M, F, A}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(M, F, [Role | A]), {M, F, A}, Role);


handle_call(get_role, _From, Role) ->
    {reply, {ok, Role}, Role};

%% 断线重连
handle_call({reconnect, Pid, Ip}, _From, Role = #role{socket_pid = OldPid, role_id = RoleID}) ->
    case get(stop) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,
    sys_conn:pack_send(OldPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    OldPid ! timeout,
    put(socket_pid, Pid),
    link(Pid),
    Pid ! {save_role_pid, self(), RoleID},
    NewRole = Role#role{socket_pid = Pid, ip = Ip},
    role_data:update_online_role(NewRole),
    boradcast_mgr:reconnect(NewRole),
    {reply, role_conver:to_login_role(NewRole), NewRole};

handle_call({reconnect, Pid, Ip, PhoneScreat}, _From, Role = #role{socket_pid = OldPid, role_id = RoleID}) ->
    case get(stop) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,
    sys_conn:pack_send(OldPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    OldPid ! timeout,
    put(socket_pid, Pid),
    link(Pid),
    Pid ! {save_role_pid, self(), RoleID},
    NewRole = Role#role{socket_pid = Pid, ip = Ip, phone_screat = PhoneScreat},
    role_data:update_online_role(NewRole),
    boradcast_mgr:reconnect(NewRole),
    {reply, role_conver:to_login_role(NewRole), NewRole};

handle_call({reconnect, Pid, Ip, OpenID, RedId, PayId, Ss}, _From, Role = #role{socket_pid = OldPid, role_id = RoleID}) ->
    case get(stop) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,
    sys_conn:pack_send(OldPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    OldPid ! timeout,
    put(socket_pid, Pid),
    link(Pid),
    Pid ! {save_role_pid, self(), RoleID},
    NewRole = Role#role{socket_pid = Pid, ip = Ip, open_id = OpenID, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
    role_data:update_online_role(NewRole),
    boradcast_mgr:reconnect(NewRole),
    {reply, role_conver:to_login_role(NewRole), NewRole};

handle_call({reconnect, Pid, Ip, OpenID, RedId, PayId, Icon, NickName, Ss}, _From, Role = #role{socket_pid = OldPid, role_id = RoleID, open_id = OpenID1}) ->
    case OpenID1 =:= "" of
        true -> 
            case get(stop) of
                undefined -> ok;
                Ref ->
                    erlang:cancel_timer(Ref)
            end,
            sys_conn:pack_send(OldPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
            OldPid ! timeout,
            put(socket_pid, Pid),
            link(Pid),
            Pid ! {save_role_pid, self(), RoleID},
            NewRole = Role#role{socket_pid = Pid, ip = Ip, open_id = OpenID, red_openid = RedId, pay_openid = PayId, icon = Icon, name = NickName, subscribe = Ss},
            role_data:update_online_role(NewRole),
            boradcast_mgr:reconnect(NewRole),
            {reply, role_conver:to_login_role(NewRole), NewRole};
        _ ->
            {reply, {false, ?error_phone_wx}, Role}
    end;

handle_call({reconnect, Pid, Ip, OpenID, RedId, PayId, Icon, NickName, Phone, PhoneScreat, Ss}, _From, Role = #role{socket_pid = OldPid, role_id = RoleID}) ->
    case get(stop) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,
    sys_conn:pack_send(OldPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    OldPid ! timeout,
    put(socket_pid, Pid),
    link(Pid),
    Pid ! {save_role_pid, self(), RoleID},
    NewRole = Role#role{socket_pid = Pid, ip = Ip, open_id = OpenID, red_openid = RedId, pay_openid = PayId, icon = Icon, name = NickName, phone = Phone, phone_screat = PhoneScreat, subscribe = Ss},
    role_data:update_online_role(NewRole),
    boradcast_mgr:reconnect(NewRole),
    {reply, role_conver:to_login_role(NewRole), NewRole};


handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({rpc, Mod, Cmd, Data, Flag}, Role = #role{socket_pid = Pid}) ->
    case Mod:handle(Cmd, Data, Role) of
        ok ->    
            {noreply, Role};
        {ok, NewRole} ->
            do_change(Cmd, NewRole, Role),
            role_data:update_online_role(NewRole),
            {noreply, NewRole#role{need_sync = true}};
        {ok, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            role_data:update_online_role(NewRole),
            sys_conn:pack_send(Pid, Cmd, Flag, Reply),
            {noreply, NewRole#role{need_sync = true}};
        {reply, Reply} ->
            sys_conn:pack_send(Pid, Cmd, Flag, Reply),
            {noreply, Role};
        {false, Reply} ->
            sys_conn:pack_send_error(Pid, Cmd, Flag, Reply),
            {noreply, Role};
        {false, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            role_data:update_online_role(NewRole),
            sys_conn:pack_send_error(Pid, Cmd, Flag, Reply),
            {noreply, NewRole#role{need_sync = true}}
    end;


handle_info(zero_flush, Role) ->
    NewRole = do_zero_flush(Role),
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), zero_flush),
    {noreply, NewRole};

handle_info(init, Role) ->
%%    NewRole = role_login:do(Role),
    self() ! loop,
    {noreply, Role};

handle_info(delete_phone, Role) ->
    {noreply, Role#role{phone = ""}};

handle_info(delete_wx, Role) ->
    {noreply, Role#role{open_id = "", red_openid = "", pay_openid = ""}};

handle_info({delete_skill, Item}, Role = #role{role_id = RoleID, skill_list = SkillList, status = Status, room_pid = Pid}) ->
    NewList = lists:keydelete(Item, #role_skill.type, SkillList),
    case Status of
        ?status_normal -> ok;
        _ ->
            Pid ! {delete_skill, RoleID, Item}
    end,
    {noreply, Role#role{skill_list = NewList}};

handle_info(task_over, Role) ->
    NewRole = task:do_task_over(Role),
    {noreply, NewRole};


%% 连接器进程异常退,只处理玩家 延迟1分钟
handle_info({'EXIT', SocketPid, _Why}, Role = #role{socket_pid = SocketPid}) ->
    Ref = erlang:send_after(60000, self(), stop),
    put(stop, Ref),
    role_data:update_online_role(Role),
    {noreply, Role};

handle_info(stop, Role) ->
    {stop, normal, Role};

%% 执行异步apply操作
handle_info({apply_async, {F}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role]), {undefined, F, []}, Role);
handle_info({apply_async, {F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role | A]), {undefined, F, A}, Role);
handle_info({apply_async, {M, F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(M, F, [Role | A]), {M, F, A}, Role);

%% 内部循环
handle_info(loop, Role = #role{role_id = RoleID, loop_counter = C, need_sync = Sync, socket_pid = SocketPid, hit_num = Hit, animal_flag = Flag, coin = Coin, vip_charge = VipCharge}) ->
    %% 约每隔180秒执行一次GC
    case C rem 18 =:= 0 of
        false -> ignore;
        true -> 
            garbage_collect()
    end,
    %% 约每隔60秒检查一次数据保存需求
    S = case C rem 6 =:= 0 andalso Sync of
        false -> Sync;
        true ->
            role_data:save(Role),
            false
    end,
    %% 每10秒进行排行榜金币排行
    case Sync of
        true -> 
            rank:handle(?rank_coin, Role);
        _ ->
            ok
    end,
    %% 约每隔2分种检查连接进程是否堆积过多的消息
    case C rem 12 =:= 0 of
        false -> ignore;
        true ->
            case erlang:is_process_alive(SocketPid) of
                true ->
                    case process_info(SocketPid, message_queue_len) of
                        {_, QueueLen} when QueueLen > 10000 ->
                            ?ERR("连接进程堆积消息过多，断开连接"),
                            erlang:exit(SocketPid, kill);
                        _ -> ignore
                    end;
                _ ->
                    ignore
            end
    end,
    %% 10秒后进行下次循环
    erlang:send_after(10000, self(), loop),
    NewRole = case C rem 2 =:= 0 of
        true ->
            case Flag of
                1 -> 
                    Role#role{animal_flag = 0};
                _ ->
                    case Hit >= 300 of
                        false -> Role#role{hit_num = 0};
                        true -> 
                            ?ERR("玩家[~w]20秒内点击数量~w", [RoleID, Hit]),
                            role_black:add_black(RoleID),
                            sys_conn:pack_send(1148, #m_1148_toc{msg = "合理游戏，拒绝外挂！"}),
                            Role#role{hit_num = 0}
                    end
            end;
        _ -> Role
    end,
    %%  内部号
    NewRole1 = case Sync of
        true ->
            case sys_env:get_env(gm_id) of
                GmList when is_list(GmList) ->
                    case lists:member(RoleID, GmList) of
                        true -> 
                            case Coin =< 500000 of
                                true -> 
                                    Add = 580,
                                    {ok, NewRole0} = role_lib:do_add_coin(NewRole, Add * 10000),
                                    NewVip = vip:get_lev(VipCharge + Add * 100),
                                    NewRole0#role{vip_charge = VipCharge + Add * 100, vip = NewVip};
                                _ ->
                                    NewRole
                            end;
                        _ -> 
                            NewRole
                    end;
                _ ->
                    NewRole
            end;
        _ ->
            NewRole
    end,
    {noreply, NewRole1#role{loop_counter = C + 1, need_sync = S}};


handle_info(kill_role, Role = #role{socket_pid = SockPid}) ->
    sys_conn:pack_send(SockPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    {stop, normal, Role};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, Role) ->
    role_out:do(Role),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 处理异步apply的返回值
handle_apply_async_return({ok, NewRole}, {M, F, _A}, Role) when is_record(NewRole, role) ->
    do_change({M, F}, NewRole, Role),
    {noreply, NewRole#role{need_sync = true}};
handle_apply_async_return(ok, _Mfa, Role) ->
    {noreply, Role};
handle_apply_async_return(NewRole, {M, F, _A}, Role) when is_record(NewRole, role) ->
    do_change({M, F}, NewRole, Role),
    {noreply, NewRole#role{need_sync = true}};
handle_apply_async_return(Else, {M, F, A}, Role) ->
    ?ERR("角色[~ts]执行{~w, ~w, ~w}时得到错误的返回值格式:~w", [Role#role.name, M, F, A, Else]),
    {noreply, Role}.

%% 处理同步apply的返回值
handle_apply_sync_return({ok, Reply, NewRole}, {M, F, _A}, Role) when is_record(NewRole, role) ->
    do_change({M, F}, NewRole, Role),
    {reply, Reply, NewRole#role{need_sync = true}};
handle_apply_sync_return({ok, Reply}, _Mfa, Role) ->
    {reply, Reply, Role};
handle_apply_sync_return(Else, {M, F, A}, Role) ->
    ?ERR("角色[~ts]同步执行{~w, ~w, ~w}时得到错误的返回值格式:~w", [Role#role.name, M, F, A, Else]),
    {reply, Else, Role}.



%%变化值统一处理
do_change(Cmd, NewRole, Role)-> 
    do_coin_change(Cmd, NewRole, Role),
    do_gold_change(Cmd, NewRole, Role),
    do_red_bag_change(Cmd, NewRole, Role),
    do_tel_fare_change(Cmd, NewRole, Role),
    do_candy_change(Cmd, NewRole, Role),
    do_lolly_change(Cmd, NewRole, Role),
    do_charge_change(Cmd, NewRole, Role),
    do_exchange_change(Cmd, NewRole, Role),
    do_lollipop_change(Cmd, NewRole, Role).


%% 金币发生变化
do_coin_change(Cmd, _NewRole = #role{role_id = RoleID, coin = Coin1}, _Role = #role{coin = Coin2}) when Coin1 =/= Coin2-> 
    do_coin_log(Cmd, RoleID, Coin2, Coin1 - Coin2, Coin1);
do_coin_change(_, _, _) -> ok.

%% 钻石发生变化
do_gold_change(Cmd, _NewRole = #role{role_id = RoleID, gold = Gold1}, _Role = #role{gold = Gold2}) when Gold1 =/= Gold2-> 
    do_gold_log(Cmd, RoleID, Gold2, Gold1 - Gold2, Gold1);
do_gold_change(_, _, _) -> ok.

%% 红包发生变化
do_red_bag_change(Cmd, _NewRole = #role{role_id = RoleID, item = #role_item{red_bag = Value1}}, _Role = #role{item = #role_item{red_bag = Value2}}) when Value1 =/= Value2-> 
    do_red_bag(Cmd, RoleID, Value2, Value1 - Value2, Value1);
do_red_bag_change(_, _, _) -> ok.

%% 话费发生变化
do_tel_fare_change(Cmd, _NewRole = #role{role_id = RoleID, item = #role_item{tel_fare = Value1}}, _Role = #role{item = #role_item{tel_fare = Value2}}) when Value1 =/= Value2-> 
    do_tel_fare(Cmd, RoleID, Value2, Value1 - Value2, Value1);
do_tel_fare_change(_, _, _) -> ok.

%% 棒棒糖发送变化
do_lollipop_change(Cmd, NewRole = #role{role_id = RoleID, item = #role_item{lollipop = Value1}}, _Role = #role{item = #role_item{lollipop = Value2}}) when Value1 =/= Value2-> 
    rank:handle(?rank_lollipop, NewRole),
    do_lollipop_log(Cmd, RoleID, Value2, Value1 - Value2, Value1);
do_lollipop_change(_Cmd, _NewRole, _Role)->  ok.

%% 糖果发生变化
do_candy_change(Cmd, _NewRole = #role{role_id = RoleID, candy = Value1}, _Role = #role{candy = Value2}) when Value1 =/= Value2-> 
    do_candy_log(Cmd, RoleID, Value2, Value1 - Value2, Value1);
do_candy_change(_, _, _) -> ok.

%% 中棒棒糖发生变化
do_lolly_change(Cmd, _NewRole = #role{role_id = RoleID, lolly = Value1}, _Role = #role{lolly = Value2}) when Value1 =/= Value2-> 
    do_lolly_log(Cmd, RoleID, Value2, Value1 - Value2, Value1);
do_lolly_change(_, _, _) -> ok.

%% 充值发生变化
do_charge_change(_, NewRole = #role{charge = Charge}, _Role = #role{charge = Charge1}) when Charge =/= Charge1->
    role_charge_exchange_account:charge(NewRole, Charge - Charge1);
do_charge_change(_, _, _) -> ok.

%% 兑换发生变化
do_exchange_change(_, NewRole = #role{exchange = Exchange}, _Role = #role{exchange = Exchange1}) when Exchange =/= Exchange1->
    role_charge_exchange_account:exchange(NewRole, Exchange - Exchange1);
do_exchange_change(_, _, _) -> ok.


%% 金币日志处理
do_coin_log(Cmd, RoleId, Coin1, Cost, Coin2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(RoleId, coin_cost_log, insert, [RoleId, Type, Coin1, Cost, Coin2, date:unixtime()]).

%% 钻石日志处理
do_gold_log(Cmd, RoleId, Gold1, Cost, Gold2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(gold_cost_log, insert, [RoleId, Type, Gold1, Cost, Gold2, date:unixtime()]).

%% 红包日志处理
do_red_bag(Cmd, RoleId, Gold1, Cost, Gold2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(red_bag_log, insert, [RoleId, Type, Gold1, Cost, Gold2, date:unixtime()]).

%% 棒棒糖日志处理
do_lollipop_log(Cmd, RoleId, Gold1, Cost, Gold2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(lollipop_log, insert, [RoleId, Type, Gold1, Cost, Gold2, date:unixtime()]).

%% 话费日志处理
do_tel_fare(Cmd, RoleId, Gold1, Cost, Gold2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(tel_fare_log, insert, [RoleId, Type, Gold1, Cost, Gold2, date:unixtime()]).

%% 糖果日志处理
do_candy_log(Cmd, RoleId, Gold1, Cost, Gold2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(candy_log, insert, [RoleId, Type, Gold1, Cost, Gold2, date:unixtime()]).

%% 中棒棒糖日志处理
do_lolly_log(Cmd, RoleId, Gold1, Cost, Gold2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(lolly_log, insert, [RoleId, Type, Gold1, Cost, Gold2, date:unixtime()]).


%% 根据cmd判断是什么类型的操作
get_cmd_type(1303) -> ?coin_cost_hit;
get_cmd_type(Cmd) when is_integer(Cmd) -> Cmd;
get_cmd_type({area, apply_reward}) -> 2;
get_cmd_type({charge, charge_callback}) -> 3;
get_cmd_type({charge, apply_send_coin}) -> 4;
get_cmd_type({web_callback, cancel_callback}) -> 5;
get_cmd_type(Cmd) -> 
    ?ERR("未知的调用:~w", [Cmd]),
    0.


%% 0点更新
do_zero_flush(Role = #role{week_kill = Kill}) -> 
    Now = date:unixtime(),
    Role1 = clean_bonus(Role),
    Role2 = tomorrow_to_today(Role1),
    Day = date:day_of_the_week(Now),
    NewKill = case Day of
        3 -> 0;
        5 -> 0;
        7 -> 0;
        _ -> Kill
    end,
    Role3 = task_week:init(Role2, Day),
    NewRole = role_achievement:handle(Role3, ?achievement_login, 1),
    NewRole#role{daily_value = [], login_time = Now, daily_kill = 0, week_kill = NewKill}.
  


%% 清除彩金数据
clean_bonus(Role) -> Role#role{bonus_num = 0, bonus_reward = 0}.

%% 把充值返利兑现
tomorrow_to_today(Role = #role{charge_reward_tomorrow = Tomorrow, charge_reward_today = Today}) ->
    Role#role{charge_reward_tomorrow = 0, charge_reward_today = Today + Tomorrow}.



