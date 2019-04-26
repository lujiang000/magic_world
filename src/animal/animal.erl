%%----------------------------------------------------
%% 动物处理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(animal).
-behaviour(gen_server).
-export([start_link/2
        ,hit/3
        ,use_item/2
        ,out_room/1
        ,get_min_coin/2
        ,get_bonus_reward/1
        ,reconnect/1
        ,push_start_bonus_reward/1
        ,do_test/0
        ,get_xy/2
        ,use_expression/3
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        id = 0
        ,type = 0
        ,role_list = []
        ,animal_list = []
        ,num = 1
        ,skill = []
        ,pre_list = []
        ,guide_task = 0
    }
).

-include("common.hrl").
-include("animal.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("rank.hrl").
-include("error_msg.hrl").


start_link(Type, Id) ->
    gen_server:start_link(?MODULE, [Type, Id], []).

%% 推送开始抽奖
push_start_bonus_reward(#role{role_id = RoleId, status = ?status_zone, room_pid = Pid}) when is_pid(Pid)->
    Pid ! {push_start_bonus, RoleId};
push_start_bonus_reward(_) ->
    {false, ?error_act}.

%% 使用表情
use_expression(Role = #role{role_id = RoleId, status = ?status_zone, room_pid = Pid}, Type, ToId) when is_pid(Pid) ->
    case role_lib:do_cost_gold(Role, 1) of
        {ok, NewRole} ->
            Pid ! {use_expression, RoleId, Type, ToId},
            {ok, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;
use_expression(_, _, _) ->
    {false, ?error_act}.

%% 打动物
hit(Role = #role{role_id = RoleId, status = ?status_zone, room_pid = Pid, room_type = Type, use_coin = CoinLev, skill_list = SkillList, luck_num = LuckNum, animal_flag = Flag, guide_task = #guide_task{id = TaskId}}, Id, Coin) when is_pid(Pid)->
    Min = get_min_coin(Type, TaskId),
    {Max, _, _} = zoo_room_power_setting:get_data(CoinLev),
    NewCoin = case lists:keyfind(rage, #role_skill.type, SkillList) of
        #role_skill{effect = Effect} ->
            trunc(Coin/Effect);
        _ ->
            Coin
    end,
    case NewCoin >= Min andalso NewCoin =< Max of
        true ->
            role_lib:send_buff_begin(),
            case role_lib:do_cost_coin(Role, Coin) of
                {ok, NewRole} ->
                    case catch gen_server:call(Pid, {hit, RoleId, Id, Coin, LuckNum, Flag}) of
                        {ok, HitList, List, NewLuck} -> 
                            {ok, NewRole1} = role_lib:do_add(NewRole, List),
                            NewRole2 = do_bonus(HitList, NewRole1, 0),
                            animal_account_mgr:update_animal_pw(Coin, List),
                            role_lib:send_buff_flush(),
                            NewRole3 = task:handle(NewRole2, animal_die, Type, HitList),
                            NewRole4 = do_daily_kill(NewRole3, List),
                            NewRole5 = task:handle_guide(NewRole4, animal_die, HitList),
                            NewRole6 = role_achievement:handle(NewRole5, ?achievement_hunt, erlang:length(HitList)),
                            NewRole7 = task_week:handle(NewRole6, animal_die, HitList),
                            NewRole8 = task_week:handle(NewRole7, win_gold, List),
                            {ok, NewRole8#role{luck_num = NewLuck}};
                        ok ->
                            role_lib:send_buff_flush(),
                            case Flag of
                                99 -> ok;
                                _ ->
                                    animal_account_mgr:update_animal_pw(Coin, 0)
                            end,
                            {ok, NewRole};
                        {false, Reason} -> 
                            role_lib:send_buff_clean(),
                            {false, Reason};
                        _ -> 
                            role_lib:send_buff_clean(),
                            {false, ?error_busy}
                    end;
                _ ->
                    role_lib:send_buff_clean(),
                    {false, ?error_coin}
            end;
        _ ->
            {false, ?error_act}
    end;
hit(_, _, _) ->
    {false, ?error_act}.


%% 每日击杀
do_daily_kill(Role = #role{daily_kill = Kill, week_kill = Kill1}, List) ->
    case lists:keyfind(coin, 1, List) of
        {coin, Coin} ->
            NewRole = Role#role{daily_kill = Kill + Coin, week_kill = Kill1 + Coin},
            rank:handle(?rank_kill, NewRole),
            rank:handle(?rank_kill_week, NewRole),
            NewRole;
        _ ->
            Role
    end.

%% 使用技能
use_item(Role = #role{role_id = RoleId, status = ?status_zone, room_pid = Pid, skill_list = List}, Item) when is_pid(Pid)->
    case lists:keyfind(Item, #role_skill.type, List) of
        false ->
            case is_process_alive(Pid) of
                true ->
                    role_lib:send_buff_begin(),
                    case check_item_num(Role, Item) of
                        {ok, NewRole} ->
                            case catch gen_server:call(Pid, {use_item, RoleId, Item}) of
                                ok ->
                                    role_lib:send_buff_flush(),
                                    NewRole1 = task:handle(NewRole, use_skill, 0, Item),
                                    {ok, NewRole1};
                                {ok, Skill} ->
                                    role_lib:send_buff_flush(),
                                    Now = date:unixtime(),
                                    do_skill(Skill, Now),
                                    NewRole1 = task:handle(NewRole, use_skill, 0, Item),
                                    {ok, NewRole1#role{skill_list = [Skill | List]}};
                                {false, Reason} ->
                                    role_lib:send_buff_clean(),
                                    {false, Reason};
                                _ ->
                                    role_lib:send_buff_clean(),
                                    {false, ?error_busy}
                            end;
                        {false, Reason} ->
                            role_lib:send_buff_clean(),
                            {false, Reason}
                    end;
                _ ->
                    {false, ?error_act}
            end;
        _ ->
            {false, ?error_act}
    end;
use_item(_, _) ->
    {false, ?error_act}.

%% 技能处理
do_skill(#role_skill{type = Type, end_time = Time}, Now) when Time > Now->
    erlang:send_after((Time - Now) * 1000, self(), {delete_skill, Type});
do_skill(_, _) -> ok.


%% 退出房间
out_room(Role = #role{role_id = RoleID, status = ?status_zone, room_pid = Pid}) when is_pid(Pid)->
    Pid ! {out, RoleID},
    {ok, Role#role{room_pid = undefined, room_type = 0, status = ?status_normal}};
out_room(Role) -> {ok, Role}.


%% 断线重连进来
reconnect(Role = #role{role_id = RoleId, status = ?status_zone, room_pid = Pid, socket_pid = SocketPid}) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> 
            case catch gen_server:call(Pid, {reconnect, RoleId, SocketPid}) of
                {ok, Data} ->
                    {ok, Data};
                _ ->
                    {false, ?error_act, Role#role{status = ?status_normal, room_pid = undefined, room_type = 0}}
            end;
        _ -> 
            {false, ?error_act, Role#role{status = ?status_normal, room_pid = undefined, room_type = 0}}
    end;
reconnect(_Role) ->  
    {false, ?error_act}.


%% 彩金抽奖
get_bonus_reward(Role = #role{role_id = RoleId, bonus_num = Num, bonus_pool = Pool, bonus_reward = N, room_pid = Pid, item = #role_item{tel_fare = Tel}}) -> 
    Need = get_bonus_num(N),
    case Num >= Need of
        true ->
            List = get_pool_reward(Pool, Tel),
            {Item, Num1, _} = case N =:= -1 of   %% 特殊处理新手任务
                true ->
                    {gold, 10, 0};
                _ ->
                    sys_rand:rand_list(List, 3)
            end,
            {ok, NewRole} = role_lib:do_add(Role, [{Item, Num1}]), 
            case is_pid(Pid) of
                true ->
                    Pid ! {push_end_bonus, RoleId, Item, Num1};
                _ ->
                    ok
            end,
            animal_account_mgr:update_bonus(Item, Num1),
            {ok, #m_1317_toc{type = Item, num = Num1, reward = N + 1}, NewRole#role{bonus_num = 0, bonus_pool = 0, bonus_reward = N + 1}};
        _ ->
            {false, ?error_act}
    end.

%% 根据已经领取的彩金次数来决定要打多少个彩金动物
get_bonus_num(-1) -> 5;
get_bonus_num(0) -> 5;
get_bonus_num(1) -> 10;
get_bonus_num(2) -> 15;
get_bonus_num(3) -> 20;
get_bonus_num(4) -> 25;
get_bonus_num(5) -> 30;
get_bonus_num(_) -> 30.


%% 根据彩金数量获取随机物品列表 前面只掉落话费
get_pool_reward(Pool, Tel) when Pool < 1000000 ->
    case Tel < 100 of
        true -> 
            [{tel_fare, 2, 10000}, {gold, 10, 150}, {coin, 200, 500}, {coin, 600, 300}];
        _ ->
            [{tel_fare, 2, 50}, {gold, 10, 150}, {coin, 200, 500}, {coin, 600, 300}]
    end;
get_pool_reward(Pool, Tel) when Pool < 4000000 ->
    case Tel < 100 of
        true ->
            [{tel_fare, 30, 10000}, {gold, 200, 150}, {coin, 8000, 800}, {coin, 240000, 300}];
        _ ->
            [{tel_fare, 30, 50}, {gold, 200, 150}, {coin, 8000, 800}, {coin, 240000, 300}]
    end;
get_pool_reward(Pool, Tel) when Pool < 12000000 ->
    case Tel < 100 of
        true ->
            [{tel_fare, 150, 10000}, {gold, 900, 150}, {coin, 35000, 800}, {coin, 980000, 300}];
        _ ->
            [{tel_fare, 150, 50}, {gold, 900, 150}, {coin, 35000, 800}, {coin, 980000, 300}]
    end;
get_pool_reward(_, _) ->
    [{tel_fare, 500, 150}, {gold, 3000, 300}, {coin, 140000, 800}, {lollipop, 6, 50}].



init([Type, Id]) ->
    State = case Type of
        {Type1, TaskId} ->
            #state{id = Id, type = Type1, guide_task = TaskId};
        _ ->
            #state{id = Id, type = Type}
    end,
    NewState = do_init(State),
    process_flag(trap_exit, true),
    erlang:send_after(1000, self(), check_animal_out),
    {ok, NewState}.

%% 打动物
handle_call({hit, RoleId, Id, Coin, Luck, Flag}, _From, State = #state{role_list = RoleList, animal_list = AnimalList}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{vip = Vip} -> 
            case lists:keyfind(Id, #animal_base.id, AnimalList) of
                Animal = #animal_base{self_id = SelfId} when SelfId =:= 0 orelse SelfId =:= RoleId->
                    push_hit(RoleId, Id, RoleList),
                    case do_hit(Animal, AnimalList, Coin, Vip, Luck, Flag, RoleId) of 
                        {HitList, NewAnimalList, Skill, ItemList, CreateNum, NewLuck} ->
                            push_die(HitList, Skill, RoleId, RoleList),
                            NewState = init_animal(CreateNum, State#state{animal_list = NewAnimalList}),
                            do_broadcast(Animal, Role, ItemList),
                            {reply, {ok, HitList, ItemList, NewLuck}, NewState};
                        _ ->
                            {reply, ok, State}
                    end;
                #animal_base{} ->
                    {reply, {false, ?error_animal_self}, State};
                _ ->
                    {reply, {false, ?error_animal_exit}, State}
            end;
        _ ->
            {reply, {false, ?error_act}, State}
    end;


%% 玩家使用道具
handle_call({use_item, RoleId, Item}, _From, State = #state{role_list = RoleList, skill = SkillList, type = Type}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{skill_id = IdList} ->
            case lists:member(Item, IdList) of
                true ->
                    {reply, {false, ?error_item_use}, State};
                _ ->
                    case lists:member(Item, SkillList) of
                        true ->
                            {reply, {false, ?error_item_repeat}, State};
                        _ ->
                            case Item =:= self_horn orelse Item =:= gold_pick of
                                true ->
                                    case Type =:= rich orelse Type =:= gold orelse Type =:= diamond orelse Type =:= single of
                                        true ->
                                            case do_use_skill(Role, Item, State) of
                                                {ok, NewState} ->
                                                    {reply, ok, NewState};
                                                {ok, Reply, NewState} ->
                                                    {reply, Reply, NewState};
                                                {false, Reason} ->
                                                    {reply, {false, Reason}, State}
                                            end;
                                        _ ->
                                            {reply, {false, ?error_act}, State}
                                    end;
                                _ ->
                                    case do_use_skill(Role, Item, State) of
                                        {ok, NewState} ->
                                            {reply, ok, NewState};
                                        {ok, Reply, NewState} ->
                                            {reply, Reply, NewState};
                                        {false, Reason} ->
                                            {reply, {false, Reason}, State}
                                    end
                            end
                    end
            end;
        _ ->
            {reply, {false, ?error_act}, State}
    end;

%% 玩家进入
handle_call({enter, Role = #animal_role{role_id = RoleId}}, _From, State = #state{type = Type, role_list = RoleList, animal_list = AnimalList}) ->
    NewList = lists:keystore(RoleId, #animal_role.role_id, RoleList, Role),
    push_in(Role, RoleList),
    NewList1 = to_p_animal_role(NewList),
    NewList2 = to_p_animal(AnimalList),
    Data = #m_1301_toc{animals = NewList2, role_list = NewList1, type = Type},
    {reply, {ok, Data}, State#state{role_list = NewList}};

%% 断线重连
handle_call({reconnect, RoleId, SocketPid}, _From, State = #state{type = Type, role_list = RoleList, animal_list = AnimalList}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{} ->
            NewList = lists:keyreplace(RoleId, #animal_role.role_id, RoleList, Role#animal_role{socket_pid = SocketPid}),
            NewList1 = to_p_animal_role(RoleList),
            NewList2 = to_p_animal(AnimalList),
            Data = #m_1318_toc{animals = NewList2, role_list = NewList1, type = Type},
            {reply, {ok, Data}, State#state{role_list = NewList}};
        _ ->
            {reply, false, State}
    end;



handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%% 检查动物是否走出去
handle_info(check_animal_out, State = #state{animal_list = List, role_list = RoleList}) ->
    {NewAnimal, OutAnimal, CreateNum} = do_animal_out(List, [], [], 0),
    push_animal_out(OutAnimal, RoleList),
    NewState = init_animal(CreateNum, State#state{animal_list = NewAnimal}),
    erlang:send_after(1000, self(), check_animal_out),
    {noreply, NewState};

%% 玩家退出 私人特殊处理
handle_info({out, _RoleId}, State = #state{type = single}) ->
    {stop, normal, State};
handle_info({out, RoleId}, State = #state{role_list = List, id = Id, type = Type}) ->
    NewList = lists:keydelete(RoleId, #animal_role.role_id, List),
    push_out(RoleId, NewList),
    animal_mgr ! {delete_room_num, Id, Type},
    {noreply, State#state{role_list = NewList}};

%% 新手任务触发
handle_info({guide_task, Id}, State = #state{type = single}) ->
    {noreply, State#state{guide_task = Id}};

%% 新的动物加进来
handle_info({add_animal, Animal}, State = #state{role_list = RoleList, animal_list = AnimalList, pre_list = PreList}) ->
    push_animal_enter([Animal], RoleList),
    List1 = lists:delete(Animal, PreList),
    {noreply, State#state{animal_list = [Animal | AnimalList], pre_list = List1}};


%% 删除技能
%% 全局 冰冻
handle_info({delete_skill, Item = ice}, State = #state{animal_list = AnimalList, skill = Skill, role_list = RoleList}) ->
    NewAnimal = [A#animal_base{status = 0}||A = #animal_base{} <-AnimalList],
    NewSkill = lists:delete(Item, Skill),
    pus_animal_status(NewAnimal, RoleList),
    {noreply, State#state{animal_list = NewAnimal, skill = NewSkill}};
%% 全局 号角
handle_info({delete_skill, Item = horn}, State = #state{skill = Skill}) ->
    NewSkill = lists:delete(Item, Skill),
    {noreply, State#state{skill = NewSkill}};


%% 删除技能
%% 私有
handle_info({delete_skill, RoleId, Item}, State = #state{role_list = RoleList}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{skill_id = Skill} ->
            push_delete_skill(RoleId, Item, RoleList),
            NewList = lists:keyreplace(RoleId, #animal_role.role_id, RoleList, Role#animal_role{skill_id = lists:keydelete(Item, #role_skill.type, Skill)}),
            {noreply, State#state{role_list = NewList}};
        _ ->
            {noreply, State}
    end;

%% 关闭房间
handle_info(stop, State) ->
    {stop, normal, State};

%% 延迟处理使用
handle_info({use_horn, BaseId}, State = #state{role_list = RoleList}) ->
    push_pre_animal(BaseId, RoleList),
    {noreply, State};

%% 推送抽奖
handle_info({push_start_bonus, RoleId}, State = #state{role_list = RoleList}) ->
    Data = #m_1319_toc{role_id = RoleId}, 
    [sys_conn:pack_send(Pid, 1319, Data)||#animal_role{socket_pid = Pid} <-RoleList],
    {noreply, State};

%% 推送使用表情
handle_info({use_expression, RoleId, Type, ToId}, State = #state{role_list = RoleList}) ->
    Data = #m_1321_toc{role_id = RoleId, type = Type, to_id = ToId}, 
    [sys_conn:pack_send(Pid, 1321, Data)||#animal_role{socket_pid = Pid} <-RoleList],
    {noreply, State};

%% 推送抽奖结果
handle_info({push_end_bonus, RoleId, Item, Num}, State = #state{role_list = RoleList}) ->
    Data = #m_1320_toc{role_id = RoleId, type = Item, num = Num}, 
    [sys_conn:pack_send(Pid, 1320, Data)||#animal_role{socket_pid = Pid} <-RoleList],
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{id = Id, type = Type}) ->
    animal_mgr ! {delete, Id, Type},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 进行广播
do_broadcast(#animal_base{name = Name, is_notice = Notice}, #animal_role{name = RoleName}, ItemList) ->
    case lists:keyfind(coin, 1, ItemList) of
        {coin, Win} when Win > 50000 andalso Notice =:= 1 ->
            Msg = case lists:keyfind(red_bag, 1, ItemList) of
                {red_bag, Red} ->
                    util:fbin("恭喜玩家 ~ts 打中一只 ~ts 获得 ~w 金币和 ~ts 元", [RoleName, Name, Win, util:to_string(Red/100)]);
                _ ->
                    util:fbin("恭喜玩家 ~ts 打中一只 ~ts 获得 ~w 金币", [RoleName, Name, Win])
            end,
            boradcast_mgr:boradcast(Msg);
        _ ->
            case lists:keyfind(red_bag, 1, ItemList) of
                {red_bag, Red} ->
                    Msg = util:fbin("恭喜玩家 ~ts 打中一只 ~ts 获得  ~ts 元", [RoleName, Name, util:to_string(Red/100)]),
                    boradcast_mgr:boradcast(Msg);
                _ ->
                    ok
            end
    end,
    case lists:keyfind(lollipop, 1, ItemList) of
        {lollipop, 1} ->
            Msg1 = util:fbin("恭喜玩家 ~ts 打中一只 ~ts 获得价值200W金币的棒棒糖x1", [RoleName, Name]),
            boradcast_mgr:boradcast(Msg1);
        {lollipop, Num} ->
            Msg1 = util:fbin("恭喜玩家 ~ts 打中一只 ~ts 触发<女神眷恋>获得价值200W金币的棒棒糖x~w", [RoleName, Name, Num]),
            boradcast_mgr:boradcast(Msg1);
        _ -> ok
    end.



%% 推送有玩家推出
push_out(RoleId, List) ->
    [sys_conn:pack_send(Pid, 1308, #m_1308_toc{role_id = RoleId})||#animal_role{socket_pid = Pid} <-List].

%% 推送玩家进入
push_in(Role = #animal_role{}, List) ->
    Role1 = to_p_animal_role(Role),
    [sys_conn:pack_send(Pid, 1307, #m_1307_toc{role = Role1})||#animal_role{socket_pid = Pid} <-List].

%% 推送玩家点击
push_hit(RoleId, Id, List) ->
    [sys_conn:pack_send(Pid, 1305, #m_1305_toc{role_id = RoleId, id = Id})||#animal_role{socket_pid = Pid} <-List].

%% 推送动物死亡
push_die(HitList, Skill, RoleId, RoleList) ->
    List = to_p_animal_die(HitList),
    Data = #m_1309_toc{role_id = RoleId, type = Skill, ids = List},
    [sys_conn:pack_send(Pid, 1309, Data)||#animal_role{socket_pid = Pid} <- RoleList].

%% 推送动物进入
push_animal_enter([], _List) -> ok;
push_animal_enter(PushList, List) ->
    NewList = to_p_animal(PushList),
    [sys_conn:pack_send(Pid, 1306, #m_1306_toc{animals = NewList})||#animal_role{socket_pid = Pid} <-List].

%% 推送动物走出
push_animal_out([], _List) -> ok;
push_animal_out(PushList, List) ->
    [sys_conn:pack_send(Pid, 1311, #m_1311_toc{id = [ Id || #animal_base{id = Id}<-PushList]})||#animal_role{socket_pid = Pid} <-List].

%% 推送预警
push_pre_animal(BaseId, List) ->
    [sys_conn:pack_send(Pid, 1310, #m_1310_toc{base_id = BaseId})||#animal_role{socket_pid = Pid} <-List].

%% 推送使用技能
push_use_skill(RoleId, Icon, Item, List) ->
    [sys_conn:pack_send(Pid, 1312, #m_1312_toc{role_id = RoleId, type = Item, icon = Icon})||#animal_role{socket_pid = Pid} <-List].
push_use_skill(RoleId, Icon, Item, Effect, List) ->
    [sys_conn:pack_send(Pid, 1312, #m_1312_toc{role_id = RoleId, type = Item, icon = Icon, effect = Effect})||#animal_role{socket_pid = Pid} <-List].

%% 推送技能消失
push_delete_skill(RoleId, Item, List) ->
    [sys_conn:pack_send(Pid, 1314, #m_1314_toc{id = RoleId, type = Item})||#animal_role{socket_pid = Pid} <-List].

%% 推送动物状态
pus_animal_status(AnimalList, List) ->
    NewList = to_p_animal_status(AnimalList),
    [sys_conn:pack_send(Pid, 1313, #m_1313_toc{list = NewList})||#animal_role{socket_pid = Pid} <-List].


%% 初始化房间
do_init(State) ->
    init_animal(20, State).


%% 批量产生动物
init_animal(0, State) -> State;
init_animal(N, State = #state{animal_list = List, role_list = RoleList, num = Num, pre_list = PreList, type = Type, guide_task = TaskId}) ->
    {PushList, NewList, NewNum, PreList1} = init_animal(N, [], List, Num, RoleList, PreList, Type, TaskId),
    push_animal_enter(PushList, RoleList),
    NewState = State#state{animal_list = NewList, num = NewNum, pre_list = PreList1},
    NewState.


%% 初始化线路
init_animal(0, List1, List, Num, _, PreList, _Type, _) -> {List1, List, Num, PreList};
init_animal(N, List1, List, Num, RoleList, PreList, Type, TaskId) ->
    Animal = #animal_base{base_id = BaseId, rate = [Min, Max]} = get_one_annimal(PreList ++ List, TaskId, Type),
    Rate = sys_rand:rand(Min, Max),
    RedBag = create_red_bag(Type, BaseId),
    #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
    case lists:member(BaseId, ?animal_pre_notice_list) of
        true ->
            NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = 0, route_id = RouteId, xy = XY, red_bag = RedBag, rate = Rate},
            push_pre_animal(BaseId, RoleList),
            erlang:send_after(3000, self(), {add_animal, NewAnimal}),   %% 预警动物之后再加载
            init_animal(N -1, List1, List, Num + 1, RoleList, [NewAnimal | PreList], Type, TaskId);
        _ ->
            NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, xy = XY, red_bag = RedBag, rate = Rate},
            init_animal(N -1, [NewAnimal | List1], [NewAnimal| List], Num + 1, RoleList,  PreList, Type, TaskId)
    end.

%% 是否产出红包
create_red_bag(Type, BaseId) ->
    case setting_mgr:get(?setting_animal) of
        {ok, 1} ->
            case lists:member(BaseId, ?red_bag_animal_list) andalso (Type =:= rich orelse Type =:= gold orelse Type =:= diamond orelse Type =:= single)of
                true ->
                    1;
                _ ->
                    0
            end;
        _ ->
            0
    end.


%% 指定产出一种动物
init_one_animal(BaseId, State = #state{animal_list =  List, role_list = RoleList, num = Num, pre_list = PreList, type = Type}, Horn) ->
    case lists:keyfind(BaseId, #animal_base.base_id, get_animal_base(Type)) of
        Animal = #animal_base{rate = [Min, Max]} ->
            Rate = sys_rand:rand(Min, Max),
            RedBag = create_red_bag(Type, BaseId),
            #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
            case lists:member(BaseId, ?animal_pre_notice_list) of
                true ->
                    NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = 0, route_id = RouteId, is_horn = Horn, xy = XY, red_bag = RedBag, rate = Rate},
                    erlang:send_after(5000, self(), {add_animal, NewAnimal}),   %% 预警动物之后再加载
                    State#state{num  = Num + 1, pre_list = [NewAnimal | PreList]};
                _ ->
                    NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, is_horn = Horn, xy = XY, red_bag = RedBag, rate = Rate},
                    push_animal_enter([NewAnimal], RoleList),
                    State#state{num = Num + 1, animal_list = [NewAnimal | List]}
            end;
        _ ->
            State
    end.

init_one_animal(BaseId, State = #state{animal_list =  List, role_list = RoleList, num = Num, pre_list = PreList, type = Type}, Horn, {RoleId, RoleName}) ->
    case lists:keyfind(BaseId, #animal_base.base_id, get_animal_base(Type)) of
        Animal = #animal_base{rate = [Min, Max]} ->
            Rate = sys_rand:rand(Min, Max),
            RedBag = create_red_bag(Type, BaseId),
            #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
            case lists:member(BaseId, ?animal_pre_notice_list) of
                true ->
                    NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = 0, route_id = RouteId, is_horn = Horn, xy = XY, red_bag = RedBag, rate = Rate, self_id = RoleId, self_name = RoleName},
                    erlang:send_after(5000, self(), {add_animal, NewAnimal}),   %% 预警动物之后再加载
                    State#state{num  = Num + 1, pre_list = [NewAnimal | PreList]};
                _ ->
                    NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, is_horn = Horn, xy = XY, red_bag = RedBag, rate = Rate, self_id = RoleId, self_name = RoleName},
                    push_animal_enter([NewAnimal], RoleList),
                    State#state{num = Num + 1, animal_list = [NewAnimal | List]}
            end;
        _ ->
            State
    end.


%% 随机产生一只动物 只允许一只动物在场上
%%get_one_annimal(List, TaskId) when TaskId >= 1 andalso TaskId =< 5 ->
%%    OnlyList = [Id || #animal_base{base_id = Id} <- List, lists:member(Id, ?animal_only_one_list)],
%%    NewList = [A||A = #animal_base{base_id = Id} <-get_task_animal(TaskId), not lists:member(Id, OnlyList)],
%%    case sys_rand:rand_list(NewList, #animal_base.pre) of
%%        Animal = #animal_base{base_id = type_bomber, bomber_type = List1} ->
%%            Type = sys_rand:rand_list(List1),
%%            #animal_base{rate = Rate} = lists:keyfind(Type, #animal_base.base_id, get_animal_base()),
%%            Animal#animal_base{bomber_type = Type, rate = Rate};
%%        _Animal -> _Animal
%%    end;
get_one_annimal(List, _TaskId, RoomType) ->
    OnlyList = [Id || #animal_base{base_id = Id} <- List, lists:member(Id, ?animal_only_one_list)],
    NewList = [A||A = #animal_base{base_id = Id} <-get_animal_base(RoomType), not lists:member(Id, OnlyList)],
    case sys_rand:rand_list(NewList, #animal_base.pre) of
        Animal = #animal_base{base_id = type_bomber, bomber_type = List1} ->
            Type = sys_rand:rand_list(List1),
            #animal_base{rate = Rate} = lists:keyfind(Type, #animal_base.base_id, get_animal_base(RoomType)),
            Animal#animal_base{bomber_type = Type, rate = Rate};
        _Animal -> _Animal
    end.



%% 根据打中的动物列表计算彩金, 并且进行推送
do_bonus([], Role, 0) -> 
    Role;
do_bonus([], Role = #role{bonus_num = Num, bonus_pool = Pool, bonus_reward = Reward}, 1) -> 
    sys_conn:pack_send(1316, #m_1316_toc{bonus = Pool, num = Num, reward = max(0, Reward)}),
    Role;
do_bonus([#animal_base{bonus = 1, drop_list = DropList} | L], Role = #role{bonus_num = Num, bonus_pool = Pool}, _Flag) ->  
    Win = case lists:keyfind(coin, #p_assets.type, DropList) of
        #p_assets{num = Coin} -> Coin;
        _ -> 0
    end,
    do_bonus(L, Role#role{bonus_num = Num + 1, bonus_pool = Pool + trunc(Win/10)}, 1);
do_bonus([_ | L], Role, Flag) ->
    do_bonus(L, Role, Flag).


%% 动物是否走出去了
do_animal_out([], NewAnimal, OutAnimal, CreateNum) -> {NewAnimal, OutAnimal, CreateNum};
do_animal_out([Animal = #animal_base{route_id = Id, end_time = _End, post = Pos, status = 0, is_horn = Horn} | L], NewAnimal, OutAnimal, CreateNum) ->
    case get_xy(Id, Pos + 1) of
        {0, 0} ->
            Num = case Horn of
                0 -> 1;
                _ -> 
                    self() ! {delete_skill, Horn},
                    0
            end,
            do_animal_out(L, NewAnimal, [Animal#animal_base{post = Pos + 1} | OutAnimal], CreateNum + Num);
        XY ->
            do_animal_out(L, [Animal#animal_base{post = Pos + 1, xy = XY} | NewAnimal], OutAnimal, CreateNum)
    end;
do_animal_out([Animal | L], NewAnimal, OutAnimal, CreateNum) ->
    do_animal_out(L, [Animal | NewAnimal], OutAnimal, CreateNum).


%% 初始化单个线路
init_animal_route() ->
    Id = sys_rand:rand_list(animal_route:get_all()),
    Post = sys_rand:rand(1, 5),
    XY = get_xy(Id, Post),
    #animal_route{id = Id, post = Post, xy = XY}.


%% 转换前端数据
to_p_animal_role(List) when is_list(List)->
    [to_p_animal_role(Role) || Role <-List];
to_p_animal_role(#animal_role{role_id = RoleId, name = Name, icon = Icon, vip = Vip, skill_id = SkillId, vip_effect = VipEffect}) ->
    Now = date:unixtime(),
    #p_animal_role{role_id = RoleId, name = Name, icon = Icon,  vip_effect = VipEffect, skill_list = [ #p_skill{type = Type, effect = Effect, time = max(0, Time - Now)}|| #role_skill{type = Type, effect = Effect, end_time = Time} <- SkillId], vip = Vip}.

to_p_animal(List) when is_list(List) ->
    [to_p_animal(Animal) || Animal <-List];
to_p_animal(#animal_base{id = Id, base_id = self_elephant, route_id = LineId, post = Point, status = Status, red_bag = RedBag, self_id = SelfId, self_name = SelfName}) ->
    #p_animal{id = Id, base_id = self_elephant, line_id = LineId, point = Point, status = Status, red_bag = RedBag, self_id = SelfId, self_name = SelfName};
to_p_animal(#animal_base{id = Id, base_id = gold_pick, route_id = LineId, post = Point, status = Status, red_bag = RedBag, self_id = SelfId, self_name = SelfName}) ->
    #p_animal{id = Id, base_id = gold_pick, line_id = LineId, point = Point, status = Status, red_bag = RedBag, self_id = SelfId, self_name = SelfName};
to_p_animal(#animal_base{id = Id, base_id = BaseId, route_id = LineId, post = Point, status = Status, red_bag = RedBag, bomber_type = 0}) ->
    #p_animal{id = Id, base_id = BaseId, line_id = LineId, point = Point, status = Status, red_bag = RedBag};
to_p_animal(#animal_base{id = Id, base_id = BaseId, route_id = LineId, post = Point, status = Status, red_bag = RedBag, bomber_type = Type}) ->
    #p_animal{id = Id, base_id = BaseId, line_id = LineId, point = Point, status = Status, red_bag = RedBag, bomber_type = Type}.

to_p_animal_die(List) when is_list(List) ->
    [to_p_animal_die(Animal) || Animal <-List];
to_p_animal_die(#animal_base{id = Id, drop_list = List}) ->
    #p_animal_die{id = Id, item_list = List}.

to_p_animal_status(List) when is_list(List) ->
    [to_p_animal_status(A)||A <-List];
to_p_animal_status(#animal_base{id = Id, status = Status}) ->
    #p_animal_status{id = Id, status = Status}.

%% 是否打爆
%% 特殊处理皮卡丘和炸弹人
do_hit(Animal = #animal_base{base_id = pikachu},  AnimalList, Coin, Vip, Luck, Flag, RoleId) ->
    NewList = lists:delete(Animal, AnimalList),
    {Rate, HitList0} = do_hit_pikachu([Animal | NewList], 0, []),
    case do_rate(Rate/10, Coin, Flag) of
        true ->
            Active = get_acitve(),
            {HitList, NewAnimalList, ItemList, CreateNum, NewLuck} = do_drop(HitList0, AnimalList, Coin, Vip, [], [], 0, Luck, Active, RoleId),
            {HitList, NewAnimalList, 1, ItemList, CreateNum, NewLuck};
        _ ->
            false
    end;
%% 全屏炸弹
do_hit(_Animal = #animal_base{base_id = bomber},  AnimalList, Coin, Vip, Luck, Flag, RoleId) ->
    {Rate, HitList0} = do_hit_bomber(AnimalList, 0, [], RoleId),
    case do_rate(Rate/10, Coin, Flag) of
        true ->
            Active = get_acitve(),
            {HitList, NewAnimalList, ItemList, CreateNum, NewLuck} = do_drop(HitList0, AnimalList, Coin, Vip, [], [], 0, Luck, Active, RoleId),
            {HitList, NewAnimalList, 2, ItemList, CreateNum, NewLuck};
        _ ->
            false
    end;
%% 同类型炸弹人
do_hit(_Animal = #animal_base{base_id = type_bomber, bomber_type = Type}, AnimalList, Coin, Vip, Luck, Flag, RoleId) ->
    {Rate, HitList0} = do_hit_type_bomber(AnimalList, 0, [], Type),
    case do_rate(Rate/10, Coin, Flag) of
        true ->
            Active = get_acitve(),
            {HitList, NewAnimalList, ItemList, CreateNum, NewLuck} = do_drop(HitList0, AnimalList, Coin, Vip, [], [], 0, Luck, Active, RoleId),
            {HitList, NewAnimalList, 2, ItemList, CreateNum, NewLuck};
        _ ->
            false
    end;
%% 局部炸弹
do_hit(_Animal = #animal_base{base_id = area_bomber, route_id = Id, post = Post},  AnimalList, Coin, Vip, Luck, Flag, RoleId) ->
    Point = get_xy(Id, Post),
    {Rate, HitList0} = do_hit_area_bomber(AnimalList, Point, [], 0, RoleId),
    case do_rate(Rate/10, Coin, Flag) of
        true ->
            Active = get_acitve(),
            {HitList, NewAnimalList, ItemList, CreateNum, NewLuck} = do_drop(HitList0, AnimalList, Coin, Vip, [], [], 0, Luck, Active, RoleId),
            {HitList, NewAnimalList, 2, ItemList, CreateNum, NewLuck};
        _ ->
            false
    end;
do_hit(Animal = #animal_base{rate = Rate}, AnimalList, Coin, Vip, Luck, Flag, RoleId) ->
    case do_rate(Rate/10, Coin, Flag) of
        true ->
            Active = get_acitve(),
            {HitList, NewAnimalList, ItemList, CreateNum, NewLuck} = do_drop([Animal], AnimalList, Coin, Vip, [], [], 0, Luck, Active, RoleId),
            {HitList, NewAnimalList, 0, ItemList, CreateNum, NewLuck};
        _ ->
            false
    end.

%% 是否有活动掉落
get_acitve() ->
    case get(new_year_active) of
        true -> true;
        false -> false;
        _ ->
            Now = date:unixtime(),
            case setting_mgr:get(?setting_new_year) of
                {ok, [Start, End]} when Now >= Start andalso Now < End  ->
                    put(new_year_active, true),
                    true;
                _ -> 
                    put(new_year_active, false),
                    false
            end
    end.

%% 计算掉落
do_drop([Animal = #animal_base{is_horn = Horn} | L], AnimalList, Coin, Vip, HitList, ItemList, CreateNum, Luck, Active, RoleId) ->
    NewAnimalList = lists:delete(Animal, AnimalList),
    NewCreateNum = case Horn of
        0 -> CreateNum + 1;
        _ -> 
            self() ! {delete_skill, Horn},
            CreateNum
    end,
    {NewAnimal, NewItemList, NewLuck} = do_drop_item(Animal, ItemList, Coin, Vip, Luck, Active, RoleId),
    do_drop(L, NewAnimalList, Coin, Vip, [NewAnimal | HitList], NewItemList, NewCreateNum, NewLuck, Active, RoleId);
do_drop([], AnimalList, _Coin, _Vip, HitList, ItemList, CreateNum, Luck, _Active, _) -> {HitList, AnimalList, ItemList, CreateNum, Luck}.

%% 根据倍率计算是否打中 true | false
%% 小于100金币的送分
%%do_rate(Rate, Coin, Flag) when Flag =:= 0 andalso Coin =< 100 ->
%%    Pre1 = case Coin of
%%        5 -> 1.2;
%%        10 -> 1.1;
%%        20 -> 1.05;
%%        40 -> 1.025;
%%        60 -> 1.016;
%%        _ -> 1
%%    end,
%%    N = sys_rand:rand(?animal_rand_num),
%%    Pre = 1/Rate * Pre1,
%%    Num = Pre * ?animal_rand_num,
%%    N =< Num;
%% 目前设置白名单为99
do_rate(Rate, _Coin, 99)->
    Pre1  = case setting_mgr:get(?setting_animal_white_pre) of
        {ok, Value} -> Value/1000;
        _ -> ?animal_white_pre
    end,
    N = sys_rand:rand(?animal_rand_num),
    Pre = 1/Rate * Pre1,
    Num = Pre * ?animal_rand_num,
    N =< Num;
%% 黑名单
do_rate(Rate, _Coin, 97)->
    Pre1  = case setting_mgr:get(?setting_animal_black_pre) of
        {ok, Value} -> Value/1000;
        _ -> ?animal_black_pre
    end,
    N = sys_rand:rand(?animal_rand_num),
    Pre = 1/Rate * Pre1,
    Num = Pre * ?animal_rand_num,
    N =< Num;
%% 薅羊毛的党处理，金币大于1000， 倍数高于50（会有红包产出）98标识为薅羊毛
do_rate(Rate, Coin, Flag)->
    case Rate >= 50 andalso Coin >= 1000 andalso Flag =:= 98 of
        true -> 
            false;
        _ ->
            Pre1  = case setting_mgr:get(?setting_animal_pre) of
                {ok, Value} -> Value/1000;
                _ -> ?animal_pre
            end,
            N = sys_rand:rand(?animal_rand_num),
            Pre = 1/Rate * Pre1,
            Num = Pre * ?animal_rand_num,
            N =< Num
    end.



do_test() ->
    do_test(0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, []).


do_test(_, _One, N, Num, Luck, Num1, Red, Candy, Lolly, Lollipop, Win, List) when Num1 > 10000-> {N, Num, Luck, Num1, Red, Candy, Lolly, Lollipop, Win, List};
do_test(A, _One, N, Num, Luck, Num1, Red, Candy, Lolly, Lollipop, Win, List) when A < 1000 -> 
    All = trunc(2000000 * 1.20), 
    Add = case sys_rand:rand(1, 100) =< 5 of
        true ->
            1;
        _ ->
            0
    end,
    do_test(A + All, _One, N, Num, Luck + Add, Num1 + 1, Red, Candy, Lolly, Lollipop, Win, List);
do_test(All, One, N, Num, Luck, Num1, Red, Candy, Lolly, Lollipop, AllWin, List) ->
    Rate = 500,
    case do_rate(Rate/10, One, 1) of
        true ->
           {Win, RedBag} =  case setting_mgr:get(?setting_animal) of
                {ok, 1} ->
                    case Rate >= 500 of
                        true -> 
                            test_red(Rate, One);
                        _ -> {trunc(Rate * One/10), 0}
                    end;
                _ -> {trunc(Rate * One/10), 0}
            end,
            Type = sys_rand:rand_list([candy, lolly, lollipop]),
            {Value, NewC, NewL, NewLo} = case Type of
                candy -> {40000, Candy + 1, Lolly, Lollipop};
                lolly -> {400000, Candy, Lolly + 1, Lollipop};
                lollipop -> {2000000, Candy, Lolly, Lollipop + 1}
            end,
            NewList = case calc_active_item(One, Rate, true, 1) of
                [] -> List;
                [#p_assets{type = Item}] ->
                    case lists:keyfind(Item, 1, List) of
                        {Item, ItemNum} -> lists:keyreplace(Item, 1, List, {Item, ItemNum + 1});
                        _ -> [{Item, 1} | List]
                    end
            end,
            case do_rate(get_lollipop_pre(Type, One, Rate), One, 1) of
                true ->
                    do_test(All + Win - One + Value, One, N + Value, Num + One, Luck, Num1, Red + RedBag, NewC, NewL, NewLo, AllWin + Win, NewList);
                _ ->
                    do_test(All + Win - One, One, N, Num + One, Luck, Num1, Red + RedBag, Candy, Lolly, Lollipop, AllWin + Win, NewList)
                   %% case Luck > 0 of
                   %%     true ->
                   %%         case sys_rand:rand(1, 100)  =:= 1 of
                   %%             true ->
                   %%                Add = sys_rand:rand(1, 36),
                   %%                do_test(All + Win - One, One, N + Add, Num + One, Luck - 1, Num1, Red + RedBag);
                   %%             _ ->
                   %%                 do_test(All + Win - One, One, N, Num + One, Luck, Num1, Red + RedBag)
                   %%         end;
                   %%     _ ->
                   %%         do_test(All + Win - One, One, N, Num + One, Luck, Num1, Red + RedBag)
                   %% end
            end;
        _ ->
            do_test(All - One, One, N, Num + One, Luck, Num1, Red, Candy, Lolly, Lollipop, AllWin, List)
    end.

test_red(Rate, Coin) ->
%%    N = sys_rand:rand(10, 30),
%%    Win = trunc(Rate * Coin/10),
%%    RedBag = min(2000, trunc(N * Win/10000)),
%%    Coin1 = Win - trunc(RedBag * 1.2 * 100),
%%    {Coin1, RedBag}.

    N = sys_rand:rand(10, 22),
    %%N = 10,
    Win = trunc(Rate * Coin/10),
    RedBag = min(2000, trunc(N * Win/100000)),
    Coin1 = Win - trunc(RedBag * 1.2 * 1000),
    {Coin1, RedBag}.

%% 计算皮卡丘闪电技能,最多打150倍动物
do_hit_pikachu([], Rate, List) -> {Rate, List};
do_hit_pikachu([_Animal = #animal_base{rate = 0} | L], Rate, List) -> 
    do_hit_pikachu(L, Rate, List);
do_hit_pikachu([_Animal = #animal_base{base_id = area_bomber} | L], Rate, List) -> 
    do_hit_pikachu(L, Rate, List);
do_hit_pikachu([Animal = #animal_base{rate = N} | L], Rate, List) -> 
    case N + Rate =< 1500 of
        true ->
            do_hit_pikachu(L, Rate + N, [Animal| List]);
        _ ->
            {Rate, List}
    end.

%% 炸弹人,计算倍率
do_hit_bomber([], Rate, List, _)-> {Rate, List}; 
do_hit_bomber([#animal_base{base_id = self_elephant, self_id = Id} | L], Rate, List, RoleId) when Id =/= RoleId-> 
    do_hit_bomber(L, Rate, List, RoleId);
do_hit_bomber([#animal_base{base_id = gold_pick, self_id = Id} | L], Rate, List, RoleId) when Id =/= RoleId-> 
    do_hit_bomber(L, Rate, List, RoleId);
do_hit_bomber([Animal = #animal_base{rate = N} | L], Rate, List, RoleId)-> 
    do_hit_bomber(L, N + Rate, [Animal | List], RoleId).

%% 同类型炸弹
do_hit_type_bomber([], Rate, List, _Type)-> {Rate, List}; 
do_hit_type_bomber([Animal = #animal_base{base_id = Type, rate = N} | L], Rate, List, Type)-> 
    do_hit_type_bomber(L, N + Rate, [Animal | List], Type);
do_hit_type_bomber([Animal = #animal_base{base_id = type_bomber, rate = N} | L], Rate, List, Type)-> 
    do_hit_type_bomber(L, N + Rate, [Animal | List], Type);
do_hit_type_bomber([_ | L], Rate, List, Type)-> 
    do_hit_type_bomber(L, Rate, List, Type).

%% 局部炸弹
do_hit_area_bomber([], _Point, List, Rate, _) -> {Rate, List};
do_hit_area_bomber([#animal_base{base_id = self_elephant, self_id = Id} | L], Point, List, Rate, RoleId) when Id =/= RoleId->
    do_hit_area_bomber(L, Point, List, Rate, RoleId);
do_hit_area_bomber([#animal_base{base_id = gold_pick, self_id = Id} | L], Point, List, Rate, RoleId) when Id =/= RoleId->
    do_hit_area_bomber(L, Point, List, Rate, RoleId);
do_hit_area_bomber([#animal_base{rate = 0} | L], Point, List, Rate, RoleId) ->
    do_hit_area_bomber(L, Point, List, Rate, RoleId);
do_hit_area_bomber([Animal = #animal_base{route_id = Id, rate = N, post = Post} | L], Point, List, Rate, RoleId) ->
    Point1 = get_xy(Id, Post),
    case util:in_circle(Point1, Point, 300) of
        true ->
            do_hit_area_bomber(L, Point, [Animal | List], Rate + N, RoleId);
        _ ->
            do_hit_area_bomber(L, Point, List, Rate, RoleId)
    end.


%% 计算所获得的金币和红包
calc_coin_red(_, Rate, Coin, 0) ->
    Win = trunc(Rate * Coin/10),
    [#p_assets{type = coin, num = Win}];
calc_coin_red(gold_pick, Rate, Coin, 1) ->
    Win = trunc(Rate * Coin/10),
    RedBag = trunc(Win * 0.8/100),
    [#p_assets{type = red_bag, num = RedBag}, #p_assets{type = coin, num = 8888}];
calc_coin_red(_, Rate, Coin, 1) ->
    N = sys_rand:rand(10, 22),
    Win = trunc(Rate * Coin/10),
    RedBag = min(2000, trunc(N * Win/10000)),
    Coin1 = Win - trunc(RedBag * 1.2 * 1000),
    [#p_assets{type = red_bag, num = RedBag}, #p_assets{type = coin, num = Coin1}].


%% 计算棒棒糖掉落
calc_lollipop(_Coin, _, Vip, Luck) when Vip < 2-> {[], Luck};
calc_lollipop(_Coin, 0, _Vip, Luck) -> {[], Luck};
calc_lollipop(Coin, _, _Vip, Luck) when Coin < 1000 -> {[], Luck};
calc_lollipop(Coin, Rate, _, Luck) ->
    Type = sys_rand:rand_list([candy, lolly, lollipop]),
    Rate1 = get_lollipop_pre(Type, Coin, Rate),
    case do_rate(Rate1, Coin, 1) of
        true ->
            {[#p_assets{type = Type, num = 1}], Luck};
        _ ->
            case Luck >= 1 of
                true ->
                    case sys_rand:rand(1, 100) =< 1 of
                        true ->
                            Num = sys_rand:rand(1, 72),
                            {[#p_assets{type = lollipop, num = Num}], Luck - 1};
                        _ ->
                            {[], Luck}
                    end;
                _ ->
                    {[], Luck}
            end
    end.

%% 计算其他道具掉落
calc_item(_, [], _Vip) -> [];
calc_item(Coin, List, Vip) ->
    N = 
    case Coin < 100 of
        true ->
            sys_rand:rand(1, 50000);
        _ ->
            sys_rand:rand(1, 10000)
    end,
    NewList = if Vip < 3 ->
            List;
        Vip < 5 ->
            [{rage, 1, 400} | List];
        true ->
            [{rage, 1, 400}, {horn, 1, 300} | List]
    end,
    case get_item(N, NewList) of
        {Item, Num} ->
            [#p_assets{type = Item, num = Num}];
        _ ->
            []
    end.

%% 合并所有道具数量,并且转换人物所需要的格式
do_sort_item([], ItemList) -> ItemList;
do_sort_item([#p_assets{type = Type, num = Num} | L], ItemList) ->
    case lists:keyfind(Type, 1, ItemList) of
        {Type, Num1} ->
            NewList = lists:keyreplace(Type, 1, ItemList, {Type, Num + Num1}),
            do_sort_item(L, NewList);
        _ ->
            do_sort_item(L, [{Type, Num} | ItemList])
    end.


%% 计算所有掉落 
do_drop_item(Animal = #animal_base{base_id = BaseId, rate = Rate, item_list = List, red_bag = RedBag}, ItemList, Coin, Vip, Luck, Active, RoleId)->
    Coins  = calc_coin_red(BaseId, Rate, Coin, RedBag),
    {Lollipops,NewLuck} = calc_lollipop(Coin, Rate, Vip, Luck),
    Items = calc_item(Coin, List, Vip),
    ActiveItems = calc_active_item(Coin, Rate, Active, RoleId),
    Active1 = get_acitve1(),
    ActiveItems1 = calc_active_item1(Coin, Rate, Active1, RoleId),
    DropList = Coins ++ Lollipops ++ Items ++ ActiveItems ++ ActiveItems1,
    NewItemList = do_sort_item(DropList, ItemList),
    {Animal#animal_base{drop_list = DropList}, NewItemList, NewLuck}.


get_acitve1() ->
    case get(girl_active) of
        true -> true;
        false -> false;
        _ ->
            Now = date:unixtime(),
            case setting_mgr:get(?setting_girl) of
                {ok, [Start, End]} when Now >= Start andalso Now < End  ->
                    put(girl_active, true),
                    true;
                _ -> 
                    put(girl_active, false),
                    false
            end
    end.

calc_active_item1(_Coin, _Rate, false, _) -> [];
calc_active_item1(_Coin, 0, _, _) -> [];
calc_active_item1(Coin, Rate, true, RoleId) ->
    case do_rate(1/(Rate/10 * (Coin*0.01/(0.1 * 10000  * 0.75))), Coin, 1) of
        true -> 
            Num = case Rate >= 1000  of
                true -> util:ceil(sys_rand:rand(1, 3) * Coin/5000);
                _ -> 1
            end,
            Name = util:fbin("女神卡x~w", [Num]),
            log_db:log(active_drop_log, insert, [RoleId, Coin, Rate, Name, Num * 1000, date:unixtime()]),
            [#p_assets{type = active_card, num = Num}];
        _ -> []
    end.


%% 计算活动掉落物品
calc_active_item(_Coin, _Rate, false, _) -> [];
calc_active_item(_Coin, 0, _, _) -> [];
calc_active_item(Coin, Rate, true, RoleId) ->
    Type = sys_rand:rand_list([xin_card,nian_card,kuai_card,le_card]),
    Value = case Type of
        le_card -> 188;
        _ -> 4
    end,
    case do_rate(1/(Rate/10 * (Coin*0.01/(Value * 10000))), Coin, 1) of
        true -> 
            log_db:log(active_drop_log, insert, [RoleId, Coin, Rate, to_type_name(Type), Value, date:unixtime()]),
            [#p_assets{type = Type, num = 1}];
        _ -> []
    end.

to_type_name(xin_card) -> "新卡";
to_type_name(nian_card) -> "年卡";
to_type_name(kuai_card) -> "快卡";
to_type_name(le_card) -> "乐卡".



%% 棒棒糖掉落几率
get_lollipop_pre(Type, Coin, Rate) when is_integer(Rate) andalso Rate > 0 andalso is_integer(Coin) andalso Coin > 0->
    Pre  = case setting_mgr:get(?setting_animal_pre) of
        {ok, Value1} -> Value1/1000;
        _ -> ?animal_pre
    end,
    LPre  = case setting_mgr:get(?setting_lollipop_pre) of
        {ok, Value2} -> Value2/1000;
        _ -> ?lollipop_pre
    end,
    Value = case Type of
        lollipop -> 200;
        lolly -> 40;
        candy -> 4
    end,
    1/(Rate/10 * (Coin*(LPre/Pre)/(Value * 10000))).

%% 获取随机物品
get_item(N, [{Item, Num, Pre} | _L]) when N =< Pre-> 
    {Item, Num};
get_item(N, [{_Item, _Num, Pre} | L]) -> 
    get_item(N - Pre, L);
get_item(_N, []) -> false.

%% 检查道具是否足够, 不足够用钻石代替
check_item_num(#role{vip = Vip}, rage) when Vip < 3->
    {false, ?error_act};
check_item_num(#role{vip = Vip}, auto) when Vip < 2->
    {false, ?error_act};
check_item_num(#role{vip = Vip}, self_horn) when Vip < 4->
    {false, ?error_act};
check_item_num(#role{vip = Vip}, gold_pick) when Vip < 2->
    {false, ?error_act};
check_item_num(#role{vip = Vip}, horn) when Vip < 5->
    {false, ?error_act};
check_item_num(Role, Item) ->
    Flag = case Item of
        gold_pick ->
            Now = date:unixtime(),
            case setting_mgr:get(?setting_gold_pick) of
                {ok, [Start, End]} when Now >= Start andalso Now < End ->
                    true;
                _ ->
                    false
            end;
        _ ->
            true
    end,
    case Flag of
        true ->
            case role_lib:do_cost(Role, [{Item, 1}]) of
                {ok, NewRole} ->
                    {ok, NewRole};
                _ ->
                    Gold = get_item_gold(Item),
                    role_lib:do_cost_gold(Role, Gold)
            end;
        _ ->
            {false, ?error_act}
    end.

%% 获取道具价格
get_item_gold(ice) -> 2;
get_item_gold(horn) -> 10;
get_item_gold(self_horn) -> 88;
get_item_gold(gold_pick) -> 99;
get_item_gold(rage) -> 20;
get_item_gold(trumpet) -> 20;
get_item_gold(locking) -> 2;
get_item_gold(auto) -> 50.

%% 处理动物园使用技能效果
%% 全屏的
%% 冰冻
do_use_skill(_Role = #animal_role{role_id = RoleId, icon = Icon}, Item = ice, State = #state{role_list = RoleList, animal_list = AnimalList, skill = Skill}) ->
    NewAnimal = [A#animal_base{status = 1}||A = #animal_base{} <-AnimalList],
    erlang:send_after(10000, self(), {delete_skill, Item}),
    push_use_skill(RoleId, Icon, Item, RoleList),
    pus_animal_status(NewAnimal, RoleList),
    {ok, State#state{animal_list = NewAnimal, skill = [Item | Skill]}};
%% 号角
do_use_skill(_Role = #animal_role{role_id = RoleId, icon = Icon}, Item = horn, State = #state{role_list = RoleList, animal_list = AnimalList, pre_list = List}) ->
    case lists:keyfind(elephant, #animal_base.base_id, List ++ AnimalList) of
        false ->
            push_use_skill(RoleId, Icon, Item, RoleList),
            erlang:send_after(2000, self(), {use_horn, elephant}),
            NewState = init_one_animal(elephant, State, horn),
            {ok, NewState};
        _ ->
            {false, ?error_horn}
    end;

%% 私有号角
do_use_skill(_Role = #animal_role{role_id = RoleId, icon = Icon, name = RoleName}, Item = self_horn, State = #state{skill = _Skill, role_list = RoleList, animal_list = AnimalList, pre_list = List}) ->
    case lists:keyfind(RoleId, #animal_base.self_id, List ++ AnimalList) of
        false ->
            push_use_skill(RoleId, Icon, Item, RoleList),
            erlang:send_after(2000, self(), {use_horn, self_elephant}),
            NewState = init_one_animal(self_elephant, State, self_horn, {RoleId, RoleName}),
            {ok, NewState};
        _ ->
            {false, ?error_self_horn}
    end;

%% 金猪
do_use_skill(_Role = #animal_role{role_id = RoleId, icon = Icon, name = RoleName}, Item = gold_pick, State = #state{skill = _Skill, role_list = RoleList, animal_list = AnimalList, pre_list = List}) ->
    case lists:keyfind(RoleId, #animal_base.self_id, List ++ AnimalList) of
        false ->
            push_use_skill(RoleId, Icon, Item, RoleList),
            erlang:send_after(2000, self(), {use_horn, gold_pick}),
            NewState = init_one_animal(gold_pick, State, gold_pick, {RoleId, RoleName}),
            {ok, NewState};
        _ ->
            {false, ?error_self_horn}
    end;

%% 私有的
%% 狂暴
do_use_skill(Role = #animal_role{role_id = RoleId, icon = Icon, skill_id = Skill}, Item = rage, State = #state{role_list = RoleList}) ->
    Now = date:unixtime(),
    N = sys_rand:rand(2, 4),
    NewSkill = #role_skill{type = Item, effect = N, end_time =  Now + 30},
    NewList = lists:keyreplace(RoleId, #animal_role.role_id, RoleList, Role#animal_role{skill_id = [NewSkill| Skill], effect = N}),
    push_use_skill(RoleId, Icon, Item, N, RoleList),
    {ok, {ok, NewSkill}, State#state{role_list = NewList}};
%% 锁定
do_use_skill(Role = #animal_role{role_id = RoleId, skill_id = Skill, icon = Icon}, Item = locking, State = #state{role_list = RoleList}) ->
    Now = date:unixtime(),
    NewSkill = #role_skill{type = Item, end_time =  Now + 30},
    NewList = lists:keyreplace(RoleId, #animal_role.role_id, RoleList, Role#animal_role{skill_id = [NewSkill | Skill]}),
    push_use_skill(RoleId, Icon, Item, RoleList),
    {ok, {ok, NewSkill}, State#state{role_list = NewList}};
%% 自动
do_use_skill(Role = #animal_role{role_id = RoleId, skill_id = Skill, icon = Icon}, Item = auto, State = #state{role_list = RoleList}) ->
    Now = date:unixtime(),
    NewSkill = #role_skill{type = Item, end_time =  Now + 1800},
    NewList = lists:keyreplace(RoleId, #animal_role.role_id, RoleList, Role#animal_role{skill_id = [NewSkill | Skill]}),
    push_use_skill(RoleId, Icon, Item, RoleList),
    {ok, {ok, NewSkill},  State#state{role_list = NewList}};
do_use_skill(_, _, _State) -> {false, ?error_act}.


%% 路线基本配置
get_animal_route() ->
    [
        #animal_route{id = 1, time = 36}
        ,#animal_route{id = 2, time = 36}
        ,#animal_route{id = 3, time = 40}
        ,#animal_route{id = 4, time = 36}
        ,#animal_route{id = 5, time = 42}
        ,#animal_route{id = 6, time = 36}
        ,#animal_route{id = 7, time = 38}
        ,#animal_route{id = 8, time = 36}
        ,#animal_route{id = 9, time = 40}
        ,#animal_route{id = 10, time = 40}
        ,#animal_route{id = 11, time = 36}
        ,#animal_route{id = 12, time = 40}
        ,#animal_route{id = 14, time = 40}
        ,#animal_route{id = 14, time = 36}
        ,#animal_route{id = 15, time = 34}
        ,#animal_route{id = 16, time = 42}
        ,#animal_route{id = 17, time = 34}
        ,#animal_route{id = 18, time = 30}
        ,#animal_route{id = 19, time = 36}
        ,#animal_route{id = 20, time = 38}
        ,#animal_route{id = 21, time = 34}
        ,#animal_route{id = 22, time = 34}
        ,#animal_route{id = 23, time = 36}
        ,#animal_route{id = 24, time = 36}
        ,#animal_route{id = 25, time = 40}
        ,#animal_route{id = 26, time = 36}
        ,#animal_route{id = 27, time = 42}
        ,#animal_route{id = 28, time = 36}
        ,#animal_route{id = 29, time = 38}
        ,#animal_route{id = 30, time = 36}
        ,#animal_route{id = 31, time = 40}
        ,#animal_route{id = 32, time = 40}
        ,#animal_route{id = 33, time = 36}
        ,#animal_route{id = 34, time = 40}
        ,#animal_route{id = 35, time = 40}
        ,#animal_route{id = 36, time = 36}
        ,#animal_route{id = 37, time = 34}
        ,#animal_route{id = 38, time = 42}
        ,#animal_route{id = 39, time = 34}
        ,#animal_route{id = 40, time = 30}
        ,#animal_route{id = 41, time = 36}
        ,#animal_route{id = 42, time = 38}
        ,#animal_route{id = 43, time = 34}
        ,#animal_route{id = 44, time = 34}

    ].
 
%% 动物基本配置
get_animal_base(petty) ->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 2000}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 1500}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 1500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 1500}
        ,#animal_base{base_id = horse,   name = "马", rate = [60, 60],         pre = 1500}
        ,#animal_base{base_id = ox,      name = "奶牛", rate = [100, 100],     pre = 1000}
        ,#animal_base{base_id = panda,   name = "熊猫", rate = [200, 200],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id  = hippo,  name = "河马", rate = [1000, 1000],   pre = 500, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = type_bomber,  name = "同类型炸弹人", rate = [0, 0], bomber_type = [cock, dog, monkey, horse, ox, panda], pre = 800}
    ];
%% 动物基本配置
get_animal_base(rich) ->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 2000}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 1500}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 1500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 1500}
        ,#animal_base{base_id = horse,   name = "马", rate = [60, 60],         pre = 1500}
        ,#animal_base{base_id = ox,      name = "奶牛", rate = [100, 100],     pre = 1000}
        ,#animal_base{base_id = panda,   name = "熊猫", rate = [200, 200],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = area_bomber,   name = "局部炸弹", rate = [300, 300], pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = xsx,   name = "小四喜", rate = [400, 400],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = dsy,   name = "大三元", rate = [600, 600],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id  = hippo,  name = "河马", rate = [1000, 1000],   pre = 500, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = lion,    name = "狮子", rate = [2000, 2000],   pre = 300, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = pikachu, name = "皮卡丘", rate = [500, 500],     pre = 500, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = type_bomber,  name = "同类型炸弹人", rate = [0, 0], bomber_type = [cock, dog, monkey, horse, ox, panda], pre = 800}
    ];
%% 动物基本配置
get_animal_base(diamond) ->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 2000}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 1500}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 1500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 1500}
        ,#animal_base{base_id = horse,   name = "马", rate = [60, 60],         pre = 1500}
        ,#animal_base{base_id = ox,      name = "奶牛", rate = [100, 100],     pre = 1000}
        ,#animal_base{base_id = panda,   name = "熊猫", rate = [200, 200],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = area_bomber,   name = "局部炸弹", rate = [300, 300], pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = xsx,   name = "小四喜", rate = [400, 400],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = dsy,   name = "大三元", rate = [600, 600],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id  = hippo,  name = "河马", rate = [1000, 1000],   pre = 500, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = lion,    name = "狮子", rate = [2000, 2000],   pre = 300, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = elephant,name = "大象", rate = [10000, 10000], pre = 100, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = self_elephant,name = "专属大象", rate = [10000, 10000], pre = 0, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = gold_pick,name = "金猪", rate = [10000, 10000], pre = 0, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = pikachu, name = "皮卡丘", rate = [500, 500],     pre = 500, is_notice = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
        ,#animal_base{base_id = bomber,  name = "炸弹人", rate = [0, 0],         pre = 200}
        ,#animal_base{base_id = type_bomber,  name = "同类型炸弹人", rate = [0, 0], bomber_type = [cock, dog, monkey, horse, ox, panda], pre = 800}
    ].

get_task_animal(_Id = 1)  ->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 500}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 3000}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 500}
        ,#animal_base{base_id = horse,   name = "马", rate =   [60, 60],       pre = 500}
        ,#animal_base{base_id = ox,      name = "奶牛", rate = [100, 100],     pre = 500}
        ,#animal_base{base_id = panda,   name = "熊猫", rate = [200, 200],     pre = 500, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
    ];
get_task_animal(Id) when Id =< 5 ->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 500}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 500}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 500}
        ,#animal_base{base_id = horse,   name = "马", rate = [60, 60],         pre = 500}
        ,#animal_base{base_id = ox,      name = "奶牛", rate = [100, 100],     pre = 500}
        ,#animal_base{base_id = panda,   name = "熊猫", rate = [200, 200],     pre = 500, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
    ];
get_task_animal(_Id = 6) ->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 2000}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 1500}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 1500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 1500}
        ,#animal_base{base_id = horse,   name = "马", rate =   [60, 60],       pre = 1500}
        ,#animal_base{base_id = ox,      name = "奶牛", rate = [100, 100],     pre = 1000}
        ,#animal_base{base_id = panda,   name = "熊猫", rate = [200, 200],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id = xsx,   name = "小四喜", rate = [400, 400],     pre = 10000, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}], bonus = 1}
    ].


%% 根据房间类型获取最低可以用多少金币玩
get_min_coin(petty, _) -> 100;
get_min_coin(rich, _) -> 1000;
get_min_coin(diamond, _) -> 20000;
get_min_coin(single, TaskId) -> 5.

get_xy(Id, Point) ->
    List = animal_route:get(Id),
    case catch lists:nth(Point, List) of
        {X, Y} when is_integer(X)-> {X, Y};
        _ -> {0, 0}
    end.

%%L = {[6639345320769292243,3608770094660354196,
%%  742243046379662650],
%% [18120842820157533730,8126736957674970839,
%%  15838874456159170949,13720667785623009027,
%%  1981124648388646073,7259762199281454546,4847305429977657164,
%%  7414450875285544033,16891263586416558696,
%%  5656958901809990669,7119686310873009186,1196684206413867236,
%%  5825020388346195561]}.
%%{_, P} = test:get_role(168192, event_pid).
%%gen_server:cast(P, {mfa_without_state, {test, info , [L, 1]}}).

