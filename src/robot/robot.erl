%%----------------------------------------------------
%% 机器人进程相关处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(robot).
-behaviour(gen_server).
-export([start_link/1
        ,handle_rpc/5
        ,do_zero_flush/1
        ,do_change/3
        ,get_gold/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("error_msg.hrl").
-include("all_pb.hrl").
-include("rank.hrl").

%% 启动角色
start_link({RoleID, N, Num}) ->
    gen_server:start_link(?MODULE, [{RoleID, N, Num}], []).


%% 玩家协议调用
handle_rpc(Pid, Mod, Cmd, Data, Flag) ->
    catch Pid ! {rpc, Mod, Cmd, Data, Flag}.


init([{RoleID, N, Num}]) ->
    process_flag(trap_exit, true),
    put(socket_pid, self()),
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), zero_flush),
    Sleep = sys_rand:rand_list([23, 1, 2]),
    erlang:send_after(date:next_diff(Sleep, 0, 0) * 1000, self(), sleep),  %% 2点睡觉
    case do_init(RoleID) of
        {ok, Role} ->
            put({rank, ?rank_kill}, {N, get_target(N)}),
            put({rank, ?rank_great_match}, {Num, get_target1(Num)}),
            put(hit_id, {0, 0}),
            put(animal_list, []),
            erlang:send_after(1000, self(), add_point),
            self() ! init,
            {ok, Role};
        _ ->
            {stop, normal}
    end.


handle_call(get_role, _From, Role) ->
    {reply, {ok, Role}, Role};


handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 接收协议
handle_info({tcp_send, Bin}, Role = #role{status = Status}) when Status =/= ?status_normal ->
    do_recieve(Role, Bin),
    {noreply, Role};

%% 打动物 
handle_info(hit, Role = #role{status = ?status_zone}) ->
    NewRole = case get(role_act) of
        hit ->
            do_hit(Role);
        _ ->
            Role
    end,
    {noreply, NewRole};



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
    self() ! do_think,
    self() ! do_something,
    {noreply, Role};

%% 开始思考, 2分钟思考一次
handle_info(do_think, Role = #role{role_id = _RoleID}) ->
    case get(role_act) of
        sleep -> ok;
        _ ->
            lookup_rank(Role, ?rank_great_match),
            case get(role_act) of
                stop ->
                    put(great_match, stop),
                    lookup_rank(Role, ?rank_kill),
                    case get(role_act) of
                        stop -> 
                            ok;
                        _ ->
                            put(role_act, go)

                    end;
                _ ->
                    put(great_match, go),
                    lookup_rank(Role, ?rank_kill),
                    case get(role_act) of
                        stop -> 
                            put(great_match, active),
                            put(role_act, go);
                        _ ->
                            ok
                    end
            end

    end,
    erlang:send_after(120000, self(), do_think),
    {noreply, Role};

%% 开始工作了
handle_info(do_something, Role) ->
    NewRole = do_something(Role),
    {noreply, NewRole};


%% 
handle_info(add_point, Role) ->
    List = get(animal_list),
    NewList = [A#p_animal{point = Point + 1} ||A = #p_animal{point = Point} <-List],
    put(animal_list, NewList),
    erlang:send_after(1000, self(), add_point),
    {noreply, Role};


%% 改变目标值
handle_info({change_target,  N, Num}, Role) ->
    put({rank, ?rank_kill}, {N, get_target(N)}),
    put({rank, ?rank_great_match}, {Num, get_target1(Num)}),
    {noreply, Role};

%% 抽奖
handle_info(bonus, Role = #role{status = ?status_zone}) ->
    animal:get_bonus_reward(Role),
    {noreply, Role};


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


%%  
handle_info({'EXIT', _SocketPid, _Why}, Role) ->
    {stop, normal, Role};

handle_info(stop, Role) ->
    {stop, normal, Role};

handle_info(sleep, Role = #role{role_id = RoleID}) ->
    {ok, _Items, NewRole} = mail_mgr:get_all_items(Role),
    put(role_act, sleep),
    Work = sys_rand:rand(8, 13),
    erlang:send_after(date:next_diff(Work, 0, 0) * 1000, self(), work),  %% 8 - 11点起来
    {noreply, NewRole};

handle_info(work, Role = #role{role_id = RoleID}) ->
    put(role_act, go),
    Sleep = sys_rand:rand_list([23, 1, 2]),
    erlang:send_after(date:next_diff(Sleep, 0, 0) * 1000, self(), sleep),  %% 23 - 2点睡觉
    {noreply, Role};


%% 内部循环, 未登陆成功的继续登陆
handle_info(loop, _Role = #role{role_id = RoleID, pid = undefined}) ->
    {ok, Role} = do_init(RoleID),
    %% 10秒后进行下次循环
    {noreply, Role};
handle_info(loop, Role = #role{loop_counter = C, need_sync = Sync, item = Item}) ->
    %% 约每隔180秒执行一次GC
    case C rem 18 =:= 0 of
        false -> ignore;
        true -> 
            garbage_collect()
    end,
    %% 约每隔60秒检查一次数据保存需求
    S = case C rem 6 =:= 0 andalso Sync of
        false -> 
            role_data:save(Role),
            Sync;
        true ->
            role_data:save(Role),
            false
    end,
    %% 每10秒进行排行榜金币排行
    rank:handle(?rank_coin, Role),
    %% 10秒后进行下次循环
    erlang:send_after(10000, self(), loop),
    {noreply, Role#role{loop_counter = C + 1, need_sync = S, item = Item#role_item{red_bag = 0}}};


handle_info(kill_role, Role = #role{socket_pid = SockPid}) ->
    sys_conn:pack_send(SockPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    {stop, normal, Role};

handle_info({use_expression, Type, RoleId}, Role) ->
    case get(use_expression) of
        Num when Num > 0 ->
            put(use_expression, Num - 1),
            do_back_expression(Role, Type, RoleId);
        _ ->
            ok
    end,
    {noreply, Role};

%% 执行异步apply操作
handle_info({apply_async, {F}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role]), {undefined, F, []}, Role);
handle_info({apply_async, {F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role | A]), {undefined, F, A}, Role);
handle_info({apply_async, {M, F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(M, F, [Role | A]), {M, F, A}, Role);


handle_info({send_msg, RoleID, Msg}, Role) ->
    role_talk:talk_to_other(Role, RoleID, Msg),
    {noreply, Role};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, Role = #role{role_id = RoleId, pid = Pid}) ->
    catch robot_mgr ! {delete, RoleId},
    case Pid of
        undefined -> ok;
        _ ->
            role_out:do(Role)
    end,
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

%%变化值统一处理
do_change(Cmd, NewRole, Role)-> 
    do_lollipop_change(Cmd, NewRole, Role).

%% 棒棒糖发送变化
do_lollipop_change(_Cmd, NewRole = #role{role_id = _RoleID, item = #role_item{lollipop = Value1}}, _Role = #role{item = #role_item{lollipop = Value2}}) when Value1 =/= Value2-> 
    rank:handle(?rank_lollipop, NewRole);
do_lollipop_change(_Cmd, _NewRole, _Role)->  ok.



%% 0点更新
do_zero_flush(Role = #role{}) -> 
    Now = date:unixtime(),
    Role1 = clean_bonus(Role),
    Role2 = tomorrow_to_today(Role1),
    Day = date:day_of_the_week(Now),
    Role2#role{daily_value = [], login_time = Now, daily_kill = 0}.
  


%% 清除彩金数据
clean_bonus(Role) -> Role#role{bonus_num = 0, bonus_reward = 0}.

%% 把充值返利兑现
tomorrow_to_today(Role = #role{charge_reward_tomorrow = Tomorrow, charge_reward_today = Today}) ->
    Role#role{charge_reward_tomorrow = 0, charge_reward_today = Today + Tomorrow}.



%% 获取人物数据
do_init(RoleId) -> 
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{}} -> 
            false;
        _ ->  
            case role_data:get_role_from_dets(RoleId) of
                {ok, Term} ->
                    case role_var:update_var(Term) of
                        {ok, Role = #role{}} ->
                            NewRole = role_login:do(Role#role{pid = self(), socket_pid = self(), use_coin = 50}),
                            put(role_act, go),
                            {ok, NewRole};
                        _Err ->
                            false
                    end;
                _Err ->
                    false
            end
    end.

%% 持续打动物
do_hit(Role = #role{room_type = Type, coin = AllCoin, role_id = RoleID}) ->
    Coin = case Type of
        gold -> 10000;
        diamond -> sys_rand:rand_list([20000, 50000, 100000, 200000]);
        _ -> 1000
    end,
    case AllCoin >= Coin of
        true -> 
            AnimalList = get(animal_list),
            case AnimalList of
                [] -> 
                    put(hit_id, {0, 0}),
                    erlang:send_after(2000, self(), hit),
                    Role;
                _ ->
                    {Id, _} = get(hit_id),
                    #p_animal{id = AnimalId, base_id = BaseId, line_id = RouteId, point = Point} = case lists:keyfind(Id, #p_animal.id, AnimalList) of
                        Animal = #p_animal{} ->
                            case lists:keyfind(self_elephant, #p_animal.base_id, AnimalList) of
                                Animal1 = #p_animal{self_id = RoleID} -> Animal1;
                                _ -> Animal
                            end;
                        _ ->
                            get_animal(RoleID, AnimalList)
                    end,
                    put(hit_id, {AnimalId, BaseId}),
                    [MaxX, MaxY] = animal_route:get_size(),
                    {X, Y} = animal:get_xy(RouteId, Point),
                    case X >= 0 andalso X =< MaxX andalso Y >= 0 andalso Y =< MaxY of
                        true ->
                            use_item(Role, BaseId),
                            case get(hit_id) of
                                {0, _} ->
                                    erlang:send_after(4000, self(), hit),
                                    Role;
                                _ ->
                                    case Type of
                                        0 ->
                                            case great_match:hit(Role, AnimalId, Coin) of
                                                {ok, NewRole} ->
                                                    case sys_rand:rand(1, 10) of              %% 有概率换目标打
                                                        1 -> put(hit_id, {0, 0});
                                                        _ -> ok
                                                    end,
                                                    erlang:send_after(300, self(), hit),
                                                    NewRole;
                                                {false, _Reason} ->
                                                    erlang:send_after(500, self(), hit),
                                                    Role
                                            end;
                                        _ ->
                                            case animal:hit(Role, AnimalId, Coin) of
                                                {ok, NewRole} ->
                                                    case sys_rand:rand(1, 10) of              %% 有概率换目标打
                                                        1 -> put(hit_id, {0, 0});
                                                        _ -> ok
                                                    end,
                                                    erlang:send_after(300, self(), hit),
                                                    NewRole;
                                                {false, _Reason} ->
                                                    erlang:send_after(500, self(), hit),
                                                    Role
                                            end
                                    end

                            end;
                        _ ->
                            erlang:send_after(1000, self(), hit),
                            Role
                    end
            end;
        _ ->
            put(role_act, go),
            animal:push_start_bonus_reward(Role),
            erlang:send_after(6000, self(), bonus),
            do_charge(Role)
    end.

%% 使用技能
use_item(Role = #role{status = Status}, BaseId) ->
    case lists:member(BaseId, [self_elephant, elephant, lion, hippo, pikachu, bomber, area_bomber]) of
        true ->
            N = case BaseId of
                self_elephant -> 
                    sys_rand:rand(1, 5000);
                elephant ->
                    sys_rand:rand(1, 8000);
                _ ->
                    sys_rand:rand(1, 10000)
            end,
            case  N =< 10 of
                true ->
                    case Status of
                        ?status_zone ->
                            animal:use_item(Role, ice);
                        _ ->
                            great_match:use_item(Role, ice)
                    end;
                _ ->
                    ok
            end;
        _ ->
            N = sys_rand:rand(1, 10000),
            case  N =< 10 of
                true ->
                    case Status of
                        ?status_zone ->
                            animal:use_item(Role, self_horn);
                        _ ->
                            great_match:use_item(Role, horn)
                    end;
                _ ->
                    ok
            end
    end.

%% 找一个动物来打, 1/4概率只打最大的， 1/4随机打， 2/4概率打中等的
get_animal(RoleID, AnimalList) ->
    case lists:keyfind(self_elephant, #p_animal.base_id, AnimalList) of
        Animal = #p_animal{self_id = RoleID} -> Animal;
        _ ->
            case sys_rand:rand(1, 4) of
                1 ->
                    case lists:keyfind(elephant, #p_animal.base_id, AnimalList) of
                        false ->
                            case lists:keyfind(bomber, #p_animal.base_id, AnimalList) of
                                false -> 
                                    case lists:keyfind(lion, #p_animal.base_id, AnimalList) of
                                        false -> sys_rand:rand_list(AnimalList);
                                        Animal -> Animal
                                    end;
                                Animal -> Animal
                            end;
                        Animal -> Animal
                    end;
                4 ->
                    sys_rand:rand_list(AnimalList);
                _ ->
                    case lists:keyfind(area_bomber, #p_animal.base_id, AnimalList) of
                        false -> 
                            case lists:keyfind(hippo, #p_animal.base_id, AnimalList) of
                                false -> 
                                    case lists:keyfind(pikachu, #p_animal.base_id, AnimalList) of
                                        false -> sys_rand:rand_list(AnimalList);
                                        Animal -> Animal
                                    end;
                                Animal -> Animal
                            end;
                        Animal -> Animal
                    end
            end
    end.



%% 没钱了先使用棒棒糖，充值588来一发, 把红包清掉
do_charge(Role = #role{coin = Coin, item = Item = #role_item{lollipop = Lollipop}}) when Lollipop > 0->
    Role#role{coin = Coin + 2000000, item = Item#role_item{lollipop = Lollipop - 1}};
do_charge(Role = #role{vip_charge = VipCharge, coin = Coin}) ->
    Num = 58800,
    NewVip = vip:get_lev(VipCharge + Num),
    Role#role{vip_charge = VipCharge + Num, coin = Coin + 5880000, vip = NewVip}.



%% 大厅状态，并且目标没有完成进房间，目标完成20%概率进房间观察下
do_something(Role = #role{status = ?status_normal}) ->
    case get(role_act) of
        go ->
            erlang:send_after(sys_rand:rand(1, 10) * 1000, self(), do_something),
            case get(great_match) of
                stop -> 
                    enter_room(Role, animal);
                active -> 
                    enter_room(Role, great);
                go -> 
                    case sys_rand:rand(1, 6) of
                        1 -> 
                            enter_room(Role, great);
                        _ ->
                            enter_room(Role, animal)
                    end
            end;
        stop ->
            erlang:send_after(30000, self(), do_something),
            case sys_rand:rand(1, 20) of
                1 -> 
                    enter_room(Role, animal);
                _ ->
                    Role
            end;
        _ ->
            erlang:send_after(30000, self(), do_something),
            Role
    end;
%% 动物园房间状态，目标没有完成开始打动物，目标完成看看，50%的概率退出房间
do_something(Role = #role{status = ?status_zone}) ->
    case get(role_act) of
        go ->
            erlang:send_after(30000, self(), do_something),
            put(role_act, hit),
            self() ! hit,
            Role;
        stop ->
            erlang:send_after(30000, self(), do_something),
            case sys_rand:rand(1, 2)  of
                1 -> 
                    out_room(Role);
                _ ->
                    Role
            end;
        hit ->
            case sys_rand:rand(1, 5)  of
                1 -> 
                    erlang:send_after(5000, self(), do_something),
                    put(role_act, go),
                    out_room(Role);
                _ ->
                    erlang:send_after(30000, self(), do_something),
                    Role
            end;
        sleep ->
            erlang:send_after(30000, self(), do_something),
            out_room(Role)
    end;

%% 大奖赛
do_something(Role = #role{status = ?status_great_match}) ->
     case get(role_act) of
        go ->
            erlang:send_after(30000, self(), do_something),
            put(role_act, hit),
            self() ! hit,
            Role;
        _ ->
            erlang:send_after(30000, self(), do_something),
            Role
    end.


%% 进入房间, 暂时处理动物园
enter_room(Role, animal) ->
    {N, _} = get({rank, ?rank_kill}),
    {Type, _} = if N =< 4 ->
            sys_rand:rand_list([{gold, 30}, {diamond, 70}], 2);
        N =< 8 ->
            sys_rand:rand_list([{gold, 50}, {diamond, 40}, {rich, 10}], 2);
        true -> 
            sys_rand:rand_list([{gold, 70}, {rich, 20}, {diamond, 10}], 2)
    end,
    case animal_mgr:enter_room(Role, Type) of
        {ok, #m_1301_toc{animals = List}, NewRole} ->
            put(animal_list, List),
            NewRole;
        {false, _Reason} ->
            Role
    end.


%% 退出房间, 暂时处理动物园
out_room(Role = #role{status = ?status_zone}) ->
    case animal:out_room(Role) of
        {ok, NewRole} ->
            put(hit_id, {0, 0}),
            NewRole;
        {false, _Reason} ->
            Role
    end;
out_room(Role = #role{status = ?status_great_match}) ->
    case great_match:out_room(Role) of
        {ok, NewRole} ->
            put(hit_id, {0, 0}),
            NewRole;
        {false, _Reason} ->
            Role
    end;
out_room(Role) ->
    Role.


%% 查看排行榜目标信息,设置新的目标值
lookup_rank(Role, Type) ->
    {N, Target} = get({rank, Type}),
    {List, Num, Value} = rank_mgr:get_rank_info(Role, 1, Type, 16, 0),
    case Num of
        0 -> %% 没有上榜 
            do_go();
        Num when Num > N ->  %% 还没有达到目标名次
            case Value >= Target of
                true ->
                    Pre = sys_rand:rand(105,  120),
                    put({rank, Type}, {N, trunc(Value * Pre/100)});
                _ ->
                    ok
            end,
            do_go();
        _ ->  %% 已经达到目标名次
            case Value >= Target of
                true ->
                    case erlang:length(List) >= N + 1 of
                        true ->
                            #p_rank_info{role_id = RoleID, value1 = Value1} = lists:nth(N + 1, List),
                            case lists:member(RoleID, sys_env:get_env(robot)) of
                                true ->  %% 自己人
                                    case N > 1 of
                                        true -> 
                                            #p_rank_info{role_id = RoleID2, value1 = Value2} = lists:nth(N - 1, List),
                                            case lists:member(RoleID2, sys_env:get_env(robot)) of
                                                true ->
                                                    put(role_act, stop);
                                                _ ->
                                                    case Value2 >= trunc(Value * 1.05) of  %%追赶下前面一名
                                                        true ->
                                                            do_go();
                                                        _ ->
                                                            put(role_act, stop)
                                                    end
                                            end;
                                        _ ->
                                            put(role_act, stop)
                                    end;
                                _ ->  %% 看看比后面一名超出没有
                                    case Value >= trunc(Value1 * 1.05) of
                                        true ->
                                            put(role_act, stop);
                                        _ ->
                                            do_go()
                                    end
                            end;
                        _ ->
                            put(role_act, stop)
                    end;
                _ ->
                    do_go()
            end
    end.


do_go() ->
    case get(role_act) of
        stop ->
            put(role_act, go);
        _ ->
            ok
    end.

%% 收到数据
do_recieve(Role, Bin) ->
    <<_ErrorID:16, _DataSize:16, DateStatus:8, _Flag:32, Cmd:16, NewBin/binary>> = Bin,
    case packet:unpack(Cmd, DateStatus, NewBin, toc) of
        {ok, Cmd, Data} ->
            case Cmd of
                1306 ->
                    #m_1306_toc{animals = List} = Data,
                    OldList = get(animal_list),
                    put(animal_list, List ++ OldList);
                1309 ->
                    #m_1309_toc{role_id = RoleID, ids = List} = Data,
                    {HitId, _} = get(hit_id),
                    OldList = get(animal_list),
                    List1 = [Id||#p_animal_die{id = Id} <-List],
                    NewList = [Animal||Animal = #p_animal{id = Id} <-OldList, not lists:member(Id, List1)],
                    put(animal_list, NewList),
                    do_use_expression(Role, HitId, List1, OldList, RoleID);
                1311 ->
                    #m_1311_toc{id = List} = Data,
                    OldList = get(animal_list),
                    NewList = [Animal||Animal = #p_animal{id = Id} <-OldList, not lists:member(Id, List)],
                    put(animal_list, NewList);
                1321 ->
                    #m_1321_toc{role_id = Id, type = Type, to_id = ToId} = Data,
                    do_back_expression(Role, Id, Type, ToId),
                    ok;
%%                1147 ->
%%                    #m_1147_toc{role_id = RoleID, message = Msg} = Data,
%%                    case check_msg(Msg) of
%%                        false -> ok;
%%                        NewMsg ->
%%                            erlang:send_after(10000, self(), {send_msg, RoleID, NewMsg})
%%                    end,
%%                    ok;
                _ ->
                    ok
            end;
        _ ->
            ?ERR("解包数据出错:~w", [Cmd])
    end.

check_msg(Msg) ->
    "你是在说:" ++ Msg ++ "?".

%% 回馈表情
do_back_expression(_Role = #role{role_id = RoleID}, Id, Type, RoleID) ->
    case sys_rand:rand(0, 14) of
        1 ->
            N = case Type of
                101 ->  1;
                102 ->  1;
                _ -> sys_rand:rand(1, 5)
            end,
            put(use_expression, N),
            erlang:send_after(4000, self(), {use_expression, Type, Id});
        _ ->
            ok
    end,
    ok;
do_back_expression(_Role, _Id, _Type, _RoleID) ->
    ok.

do_back_expression(Role = #role{status = Status}, Type, RoleID) ->
    NewType = case Type of
        101 -> sys_rand:rand_list([101, 102]);
        102 -> sys_rand:rand_list([101, 102]);
        103 -> sys_rand:rand_list([103, 104]);
        104 -> sys_rand:rand_list([103, 104])
    end,
    case Status of
        ?status_zone ->
            animal:use_expression(Role, NewType, RoleID);
        _ ->
            great_match:use_expression(Role, NewType, RoleID)
    end,
    erlang:send_after(2000, self(), {use_expression, Type, RoleID}).


%% 使用表情
do_use_expression(_Role = #role{role_id = RoleID}, _, _, _, RoleID) ->
    ok;
do_use_expression(_Role, Id, List, AnimalList, RoleID) ->
    case lists:member(Id, List) of
        true -> 
            case lists:keyfind(Id, #p_animal.id, AnimalList) of
                #p_animal{base_id = BaseId} ->
                    case lists:member(BaseId, [elephant, lion, hippo, pikachu, bomber, area_bomber]) of
                        true ->
                            case sys_rand:rand(1, 8) of
                                1 ->
                                    Type = sys_rand:rand(103, 104),
                                    put(use_expression, sys_rand:rand(1, 2)),
                                    erlang:send_after(sys_rand:rand(3, 7) * 1000, self(), {use_expression, Type, RoleID});
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.


%% 动物园每个名次最低要求值
get_target(1) -> 800000000;
get_target(2) -> 730000000;
get_target(3) -> 690000000;
get_target(4) -> 610000000;
get_target(5) -> 550000000;
get_target(6) -> 480000000;
get_target(7) -> 330000000;
get_target(8) -> 280000000;
get_target(9) -> 230000000;
get_target(_) -> 200000000.


%% 大奖赛每个名次最低要求值
get_target1(1) -> 8000000;
get_target1(2) -> 7800000;
get_target1(3) -> 7600000;
get_target1(4) -> 6200000;
get_target1(5) -> 6000000;
get_target1(6) -> 5800000;
get_target1(7) -> 5600000;
get_target1(8) -> 5400000;
get_target1(9) -> 5200000;
get_target1(_) -> 5000000.

get_gold(N) ->
    get_gold(N, 0).

get_gold(0, All) -> All;
get_gold(N, All) ->
    Gold = min(500, (N - 1) * 50),
    get_gold(N - 1, All + Gold).
