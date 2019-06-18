%%----------------------------------------------------
%%  牧场活动进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(farm_animal).
-behaviour(gen_server).
-export([start_link/6
        ,out/1
        ,hit/3
        ,use_item/2
        ,reconnect/1
        ,use_expression/3
        ,get_animal_base/0
        ,get_type_coin/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        id = 0
        ,type = 0
        ,role_list = []
        ,animal_list = []
        ,num = 1
        ,skill = []
        ,pre_list = []
        ,manager_pid 
        ,animal_num = 0
        ,death_list = []
        ,p = 0
        ,w = 0
        ,role_id = 0
        ,name = ""

    }
).

-include("common.hrl").
-include("all_pb.hrl").
-include("animal.hrl").
-include("role.hrl").
-include("error_msg.hrl").


start_link(Type, Id, List, Pid, RoleId, Name) ->
    gen_server:start_link(?MODULE, [Type, Id, List, Pid, RoleId, Name], []).

%% 退出牧场
out(Role = #role{role_id = RoleId, status = ?status_farm, room_pid = Pid}) when is_pid(Pid)->
    Pid ! {out, RoleId},
    Role#role{status = ?status_normal, room_pid = undefined, room_type = 0};
out(Role) -> Role.

%% 断线重连
%% 断线重连进来
reconnect(Role = #role{role_id = RoleId, status = ?status_farm, room_pid = Pid, socket_pid = SocketPid}) when is_pid(Pid) ->
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

%% 使用表情
use_expression(Role = #role{role_id = RoleId, status = ?status_farm, room_pid = Pid}, Type, ToId) when is_pid(Pid) ->
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
hit(Role = #role{role_id = RoleId, status = ?status_farm, room_pid = Pid}, Id, Coin) when is_pid(Pid) ->
    role_lib:send_buff_begin(),
    case role_lib:do_cost_coin(Role, Coin) of
        {ok, NewRole} ->
            case catch gen_server:call(Pid, {hit, RoleId, Id, Coin}) of
                {ok, List} -> 
                    List1 = [{Type, Num}||#p_assets{type = Type, num = Num} <-List],
                    {ok, NewRole1} = role_lib:do_add(NewRole, List1),
                    role_lib:send_buff_flush(),
                    {ok, NewRole1};
                ok ->
                    role_lib:send_buff_flush(),
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
hit(_, _, _) -> {false, ?error_act}.

%% 使用技能
use_item(Role = #role{role_id = RoleId, status = ?status_farm, room_pid = Pid, skill_list = List}, Item) when is_pid(Pid)->
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
                                    {ok, NewRole};
                                {ok, Skill} ->
                                    role_lib:send_buff_flush(),
                                    Now = date:unixtime(),
                                    do_skill(Skill, Now),
                                    {ok, NewRole#role{skill_list = [Skill | List]}};
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




init([Type, Id, List, Pid, RoleId, Name]) ->
    process_flag(trap_exit, true),
    State = #state{id = Id, type = Type, manager_pid = Pid, role_id = RoleId, name = Name},
    NewState = init_animal(List, State),
    erlang:send_after(2000, self(), check_animal_num),
    {ok, NewState}.

%% 打动物
handle_call({hit, RoleId, Id, Coin}, _From, State = #state{manager_pid = Pid, role_list = RoleList, animal_list = AnimalList, animal_num = Num, death_list = DeathList, p = P, w = W, name = RoomName, role_id = RoomRoleId, type = Type}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{name = HitName} -> 
            case lists:keyfind(Id, #animal_base.id, AnimalList) of
                Animal = #animal_base{self_id = SelfId, base_id = BaseId} when SelfId =:= 0 orelse SelfId =:= RoleId->
                    push_hit(RoleId, Id, RoleList),
                    case do_hit(Animal, AnimalList, Coin, DeathList, W) of 
                        {HitList, NewAnimalList, Skill, ItemList, CreateNum, NewDeathList, NewW} ->
                            do_notice(RoomRoleId, Type, BaseId, HitName),
                            push_die(HitList, Skill, RoleId, RoleList),
                            do_death_log(HitList, Role, RoomRoleId, Type, date:unixtime()),
                            NewState = State#state{animal_list = NewAnimalList, animal_num = Num - CreateNum, death_list = NewDeathList, p = P + Coin, w = NewW},
                            Pid ! {death_log, {RoomRoleId, Type, HitList}},
                            do_broadcast(Animal, Role, ItemList, RoomName),
                            {reply, {ok, ItemList}, NewState};
                        _ ->
                            {reply, ok, State#state{p = P + Coin}}
                    end;
                #animal_base{} ->
                    {reply, {false, ?error_animal_self}, State};
                _ ->
                    {reply, {false, ?error_animal_exit}, State}
            end;
        _ ->
            {reply, {false, ?error_act}, State}
    end;



%% 玩家进入
handle_call({enter, Role = #animal_role{role_id = RoleId}}, _From, State = #state{name = Name, type = Type, role_list = RoleList, animal_list = AnimalList, role_id = RoleId1}) ->
    NewList = lists:keystore(RoleId, #animal_role.role_id, RoleList, Role),
    push_in(Role, RoleList),
    NewList1 = to_p_animal_role(NewList),
    NewList2 = to_p_animal(AnimalList),
    Data = #m_2103_toc{animals = NewList2, role_list = NewList1, type = Type, role_id = RoleId1, name = Name},
    {reply, {ok, Data}, State#state{role_list = NewList}};

%% 玩家使用道具
handle_call({use_item, RoleId, Item}, _From, State = #state{role_list = RoleList}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{skill_id = IdList} ->
            case lists:member(Item, IdList) of
                true ->
                    {reply, {false, ?error_item_use}, State};
                _ ->
                    case do_use_skill(Role, Item, State) of
                        {ok, NewState} ->
                            {reply, ok, NewState};
                        {ok, Reply, NewState} ->
                            {reply, Reply, NewState};
                        {false, Reason} ->
                            {reply, {false, Reason}, State}
                    end
            end;
        _ ->
            {reply, {false, ?error_act}, State}
    end;

%% 断线重连
handle_call({reconnect, RoleId, SocketPid}, _From, State = #state{type = Type, role_list = RoleList, animal_list = AnimalList, role_id = RoleId1, name = Name}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{} ->
            NewList = lists:keyreplace(RoleId, #animal_role.role_id, RoleList, Role#animal_role{socket_pid = SocketPid}),
            NewList1 = to_p_animal_role(RoleList),
            NewList2 = to_p_animal(AnimalList),
            Data = #m_2114_toc{animals = NewList2, role_list = NewList1, type = Type, role_id = RoleId1, name = Name},
            {reply, {ok, Data}, State#state{role_list = NewList}};
        _ ->
            {reply, false, State}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 玩家退出
handle_info({out, RoleId}, State = #state{role_list = List, id = Id, type = Type, manager_pid = Pid}) ->
    NewList = lists:keydelete(RoleId, #animal_role.role_id, List),
    push_out(RoleId, NewList),
    Pid ! {delete_room_num, Id, Type},
    {noreply, State#state{role_list = NewList}};

%% 推送使用表情
handle_info({use_expression, RoleId, Type, ToId}, State = #state{role_list = RoleList}) ->
    Data = #m_2115_toc{role_id = RoleId, type = Type, to_id = ToId}, 
    [sys_conn:pack_send(Pid, 2115, Data)||#animal_role{socket_pid = Pid} <-RoleList],
    {noreply, State};

%% 新的动物加进来
handle_info({add_animal, Animal}, State = #state{role_list = RoleList, animal_list = AnimalList, pre_list = PreList}) ->
    push_animal_enter([Animal], RoleList),
    List1 = lists:delete(Animal, PreList),
    {noreply, State#state{animal_list = [Animal | AnimalList], pre_list = List1}};

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

%% 检查动物是否充足
handle_info(check_animal_num, State = #state{type = Type, manager_pid = Pid, animal_num = Num, death_list = DeathList, p = P, w = W})->
    case catch gen_server:call(Pid, {create_animal, 15 - Num, Type, P, W, DeathList}) of
        {ok, [], _} ->
            erlang:send_after(2000, self(), check_animal_num),
            {noreply, State#state{p = 0, w = 0}};
        {ok, List, NewDeathList} ->
            %%?ERR("have:~w:~w", [Num, Type]),
            NewState = init_animal(List, State),
            erlang:send_after(2000, self(), check_animal_num),
            {noreply, NewState#state{p = 0, w = 0, death_list = NewDeathList}};
        _ ->
            erlang:send_after(2000, self(), check_animal_num),
            {noreply, State#state{p = 0, w = 0}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{animal_list = List, id = Id, type = Type, manager_pid = Pid, p = P, w = W, pre_list = List1}) ->
    Pid ! {close, Type, Id, [BaseId|| #animal_base{base_id = BaseId}<-List1 ++ List], P, W},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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

%% 是否打爆
%% 特殊处理皮卡丘
do_hit(Animal = #animal_base{base_id = pikachu, rate = Rate1},  AnimalList, Coin, DeathList, W) ->
    NewList = lists:delete(Animal, AnimalList),
    All = util:ceil(Rate1/50 * 150),
    {Rate, HitList0, NewDeathList} = do_hit_pikachu([Animal | NewList], 0, [], DeathList, All),
    case do_rate(Rate, Coin) of
        true ->
            {HitList, NewAnimalList, ItemList, CreateNum} = do_drop(HitList0, AnimalList, [], [], 0),
            {HitList, NewAnimalList, 1, ItemList, CreateNum, NewDeathList, W + Rate};
        _ ->
            false
    end;
do_hit(Animal = #animal_base{rate = Rate, base_id = BaseId}, AnimalList, Coin, DeathList, W) ->
    case do_rate(Rate, Coin) of
        true ->
            NewDeathList = [BaseId | DeathList],
            {HitList, NewAnimalList, ItemList, CreateNum} = do_drop([Animal], AnimalList, [], [], 0),
            {HitList, NewAnimalList, 0, ItemList, CreateNum, NewDeathList, W + Rate};
        _ ->
            false
    end.

%% 掉落概率
do_rate(Rate, Coin) ->
    Pre1  = case setting_mgr:get(?setting_animal_pre) of
        {ok, Value} -> Value/1000;
        _ -> ?animal_pre
    end,
    N = sys_rand:rand(?animal_rand_num),
    Pre = Coin/Rate * Pre1,
    Num = Pre * ?animal_rand_num,
    N =< Num.

%% 计算掉落
do_drop([Animal = #animal_base{rate = Rate, red_bag = RedBag} | L], AnimalList, HitList, ItemList, CreateNum) ->
    NewAnimalList = lists:delete(Animal, AnimalList),
    NewCreateNum  =  CreateNum + 1,
    Coins  = calc_coin_red(Rate, RedBag),
    NewAnimal = Animal#animal_base{drop_list = Coins},
    NewItemList = Coins ++ ItemList,
    do_drop(L, NewAnimalList, [NewAnimal | HitList], NewItemList, NewCreateNum);
do_drop([], AnimalList, HitList, ItemList, CreateNum) -> {HitList, AnimalList, ItemList, CreateNum}.


%% 计算所获得的金币和红包
calc_coin_red(Rate, 0) ->
    [#p_assets{type = coin, num = Rate}];
calc_coin_red(Rate, 1) ->
    N = sys_rand:rand(10, 22),
    Win = Rate,
    RedBag = min(2000, trunc(N * Win/100000)),
    Coin1 = Win - trunc(RedBag * 1.2 * 1000),
    [#p_assets{type = red_bag, num = RedBag}, #p_assets{type = coin, num = Coin1}].


%% 计算皮卡丘闪电技能,最多打150倍动物
do_hit_pikachu([], Rate, List, NewList, _) -> {Rate, List, NewList};
do_hit_pikachu([Animal = #animal_base{base_id = BaseId, rate = N} | L], Rate, List, DeathList, All) -> 
    case N + Rate =< All of
        true ->
            NewList = [BaseId | DeathList],
            do_hit_pikachu(L, Rate + N, [Animal| List], NewList, All);
        _ ->
            {Rate, List, DeathList}
    end.


%% 进行广播
do_broadcast(#animal_base{name = Name, is_notice = Notice}, #animal_role{name = RoleName}, ItemList, RoomName) ->
    case lists:keyfind(coin, #p_assets.type, ItemList) of
        #p_assets{num = Win} when Win > 50000 andalso Notice =:= 1 ->
            Msg = case lists:keyfind(red_bag, #p_assets.type, ItemList) of
                #p_assets{num = Red} ->
                    Red1 = case Red rem 10 of
                        0 -> util:to_string(erlang:trunc(Red/10));
                        _ -> util:to_string(Red/10)
                    end,
                    util:fbin("恭喜玩家 ~ts 在[~ts的牧场]中打中一只 ~ts 获得 ~w 金币和 ~ts 福袋", [RoleName, RoomName, Name, Win, Red1]);
                _ ->
                    util:fbin("恭喜玩家 ~ts 在[~ts的牧场]打中一只 ~ts 获得 ~w 金币", [RoleName, RoomName, Name, Win])
            end,
            boradcast_mgr:boradcast(Msg);
        _ ->
            case lists:keyfind(red_bag, #p_assets.type, ItemList) of
                #p_assets{num = Red} ->
                    Red1 = case Red rem 10 of
                        0 -> util:to_string(erlang:trunc(Red/10));
                        _ -> util:to_string(Red/10)
                    end,
                    Msg = util:fbin("恭喜玩家 ~ts 在[~ts的牧场]打中一只 ~ts 获得  ~ts 福袋", [RoleName, RoomName, Name, Red1]),
                    boradcast_mgr:boradcast(Msg);
                _ ->
                    ok
            end
    end.
   

%% 批量产生动物
init_animal([], State) -> State;
init_animal(IdList, State = #state{animal_list = List, role_list = RoleList, num = Num, pre_list = PreList, type = Type, animal_num = AnimalNum}) ->
    {PushList, NewList, NewNum, PreList1} = init_animal(IdList, [], List, Num, RoleList, PreList, Type),
    push_animal_enter(PushList, RoleList),
    NewState = State#state{animal_list = NewList, num = NewNum, pre_list = PreList1, animal_num = erlang:length(IdList) + AnimalNum},
    NewState.

%% 初始化线路
init_animal([], List1, List, Num, _, PreList, _Type) -> {List1, List, Num, PreList};
init_animal([BaseId | L], List1, List, Num, RoleList, PreList, Type) ->
    Animal = #animal_base{base_id = BaseId, rate = Rate1} = get_one_annimal(BaseId),
    Rate = util:ceil(Rate1 * get_type_coin(Type)),
    RedBag = create_red_bag(Type, BaseId),
    #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
    case lists:member(BaseId, ?animal_pre_notice_list) of
        true ->
            NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = 0, route_id = RouteId, xy = XY, red_bag = RedBag, rate = Rate},
            push_pre_animal(BaseId, RoleList),
            erlang:send_after(3000, self(), {add_animal, NewAnimal}),   %% 预警动物之后再加载
            init_animal(L, List1, List, Num + 1, RoleList, [NewAnimal | PreList], Type);
        _ ->
            NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, xy = XY, red_bag = RedBag, rate = Rate},
            init_animal(L, [NewAnimal | List1], [NewAnimal| List], Num + 1, RoleList,  PreList, Type)
    end.


%% 获取最低金币消耗
get_type_coin(1) -> 1000;
get_type_coin(2) -> 5000;
get_type_coin(3) -> 10000.

%%获取一只动物的信息 
get_one_annimal(Id) ->
    List = get_animal_base(),
    lists:keyfind(Id, #animal_base.base_id, List).

    

%% 动物基本配置
get_animal_base() ->
    [
        #animal_base{base_id  = cock,    name = "小鸡",   rate = 1.2}
        ,#animal_base{base_id = dog,     name = "小狗",   rate = 2}
        ,#animal_base{base_id = monkey,  name = "猴子",   rate = 4}
        ,#animal_base{base_id = horse,   name = "马",     rate = 6}
        ,#animal_base{base_id = ox,      name = "奶牛",   rate = 10}
        ,#animal_base{base_id = panda,   name = "熊猫",   rate = 20}
        ,#animal_base{base_id = xsx,     name = "小四喜", rate = 40}
        ,#animal_base{base_id = dsy,     name = "大三元", rate = 60}
        ,#animal_base{base_id = hippo,   name = "河马",   rate = 100,   is_notice = 1}
        ,#animal_base{base_id = lion,    name = "狮子",   rate = 200,   is_notice = 1}
        ,#animal_base{base_id = elephant,name = "大象",   rate = 1000,  is_notice = 1}
        ,#animal_base{base_id = pikachu, name = "皮卡丘", rate = 50,    is_notice = 1}
    ].

%% 初始化单个线路
init_animal_route() ->
    Id = sys_rand:rand_list(animal_route:get_all()),
    Post = sys_rand:rand(1, 5),
    XY = get_xy(Id, Post),
    #animal_route{id = Id, post = Post, xy = XY}.

%% 获取xy点
get_xy(Id, Point) ->
    List = animal_route:get(Id),
    case catch lists:nth(Point, List) of
        {X, Y} when is_integer(X)-> {X, Y};
        _ -> {0, 0}
    end.

%% 是否产出红包
create_red_bag(_Type, BaseId) ->
    case setting_mgr:get(?setting_animal) of
        {ok, 1} ->
            case lists:member(BaseId, ?red_bag_animal_list) of
                true ->
                    1;
                _ ->
                    0
            end;
        _ ->
            0
    end.
    
%% 技能处理
do_skill(#role_skill{type = Type, end_time = Time}, Now) when Time > Now->
    erlang:send_after((Time - Now) * 1000, self(), {delete_skill, Type});
do_skill(_, _) -> ok.

%%扣除道具
check_item_num(Role, Item) ->
    case role_lib:do_cost(Role, [{Item, 1}]) of
        {ok, NewRole} ->
            {ok, NewRole};
        _ ->
            Gold = get_item_gold(Item),
            role_lib:do_cost_gold(Role, Gold)
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


%% 推送有玩家退出
push_out(RoleId, List) ->
    [sys_conn:pack_send(Pid, 2109, #m_2109_toc{role_id = RoleId})||#animal_role{socket_pid = Pid} <-List].

%% 推送玩家进入
push_in(Role = #animal_role{}, List) ->
    Role1 = to_p_animal_role(Role),
    [sys_conn:pack_send(Pid, 2108, #m_2108_toc{role = Role1})||#animal_role{socket_pid = Pid} <-List].

%% 推送玩家点击
push_hit(RoleId, Id, List) ->
    [sys_conn:pack_send(Pid, 2107, #m_2107_toc{role_id = RoleId, id = Id})||#animal_role{socket_pid = Pid} <-List].

%% 推送动物死亡
push_die(HitList, Skill, RoleId, RoleList) ->
    List = to_p_animal_die(HitList),
    Data = #m_2110_toc{role_id = RoleId, type = Skill, ids = List},
    [sys_conn:pack_send(Pid, 2110, Data)||#animal_role{socket_pid = Pid} <- RoleList].


%% 推送动物进入
push_animal_enter([], _List) -> ok;
push_animal_enter(PushList, List) ->
    NewList = to_p_animal(PushList),
    [sys_conn:pack_send(Pid, 2118, #m_2118_toc{animals = NewList})||#animal_role{socket_pid = Pid} <-List].

%% 推送预警
push_pre_animal(BaseId, List) ->
    [sys_conn:pack_send(Pid, 2111, #m_2111_toc{base_id = BaseId})||#animal_role{socket_pid = Pid} <-List].

%% 推送使用技能
push_use_skill(RoleId, Icon, Item, List) ->
    [sys_conn:pack_send(Pid, 2112, #m_2112_toc{role_id = RoleId, type = Item, icon = Icon})||#animal_role{socket_pid = Pid} <-List].

%% 推送技能消失
push_delete_skill(RoleId, Item, List) ->
    [sys_conn:pack_send(Pid, 2119, #m_2119_toc{id = RoleId, type = Item})||#animal_role{socket_pid = Pid} <-List].

%% 推送动物状态
pus_animal_status(AnimalList, List) ->
    NewList = to_p_animal_status(AnimalList),
    [sys_conn:pack_send(Pid, 2113, #m_2113_toc{list = NewList})||#animal_role{socket_pid = Pid} <-List].


%% 转换前端数据
to_p_animal_role(List) when is_list(List)->
    [to_p_animal_role(Role) || Role <-List];
to_p_animal_role(#animal_role{role_id = RoleId, name = Name, icon = Icon, vip = Vip, skill_id = SkillId, vip_effect = VipEffect}) ->
    Now = date:unixtime(),
    #p_animal_role{role_id = RoleId, name = Name, icon = Icon,  vip_effect = VipEffect, skill_list = [ #p_skill{type = Type, effect = Effect, time = max(0, Time - Now)}|| #role_skill{type = Type, effect = Effect, end_time = Time} <- SkillId], vip = Vip}.

to_p_animal_die(List) when is_list(List) ->
    [to_p_animal_die(Animal) || Animal <-List];
to_p_animal_die(#animal_base{id = Id, drop_list = List}) ->
    #p_animal_die{id = Id, item_list = List}.

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

to_p_animal_status(List) when is_list(List) ->
    [to_p_animal_status(A)||A <-List];
to_p_animal_status(#animal_base{id = Id, status = Status}) ->
    #p_animal_status{id = Id, status = Status}.

%% 动物死亡日志
do_death_log([#animal_base{base_id = BaseId} | L], Role = #animal_role{role_id = RoleId, name = Name}, RoomRoleId, Type, Time) ->
    log_db:log(farm_death_log, insert, [RoomRoleId, Type, farm:to_animal_type_integer(BaseId), 1, RoleId, Name, Time]),
    do_death_log(L, Role, RoomRoleId, Type, Time);
do_death_log([], _, _, _, _) -> ok.

%% 通知牧场主
do_notice(RoleId, Type, BaseId, Name) ->
    case ets:lookup(online_role, RoleId) of
        [#online_role{socket_pid = Pid}] -> 
            sys_conn:pack_send(Pid, 2124, #m_2124_toc{type = Type, name = Name, base_id = BaseId});
        _ ->
            ok
    end.


