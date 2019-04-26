%%----------------------------------------------------
%% 动物指引处理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(animal_guide).
-behaviour(gen_server).
-export([start_link/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        id = 0
        ,type = 0
        ,role_list = []
        ,animal_list = []
        ,num = 0
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


init([{Type, TaskId}, Id]) ->
    NewState = do_init(#state{id = Id, type = Type, guide_task = TaskId}),
    process_flag(trap_exit, true),
    erlang:send_after(1000, self(), check_animal_out),
    {ok, NewState}.

%% 打动物
handle_call({hit, RoleId, Id, Coin, Luck, Flag}, _From, State = #state{role_list = RoleList, animal_list = AnimalList}) ->
    case lists:keyfind(RoleId, #animal_role.role_id, RoleList) of
        Role = #animal_role{vip = Vip} -> 
            case lists:keyfind(Id, #animal_base.id, AnimalList) of
                Animal = #animal_base{}->
                    case do_hit(Animal, AnimalList, Coin, Vip, Luck, Flag, RoleId) of 
                        {HitList, NewAnimalList, Skill, ItemList, CreateNum, NewLuck} ->
                            push_die(HitList, Skill, RoleId, RoleList),
                            NewState = init_animal(CreateNum, State#state{animal_list = NewAnimalList}),
                            do_broadcast(Animal, Role, ItemList),
                            {reply, {ok, HitList, ItemList, NewLuck}, NewState};
                        _ ->
                            {reply, ok, State}
                    end;
                _ ->
                    {reply, {false, ?error_animal_exit}, State}
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
handle_info(check_animal_out, State = #state{animal_list = List, role_list = RoleList, guide_task = TaskId}) ->
    {NewAnimal, OutAnimal, CreateNum} = do_animal_out(List, [], [], 0, TaskId),
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
    State1 = State#state{guide_task = Id},
    NewState = case Id of
        2 ->
            init_animal(14, State1);
        3 ->
            init_animal(1, State1);
        _ ->
            State1
    end,
    {noreply, NewState};

%% 新的动物加进来
handle_info({add_animal, Animal}, State = #state{role_list = RoleList, animal_list = AnimalList, pre_list = PreList}) ->
    push_animal_enter([Animal], RoleList),
    List1 = lists:delete(Animal, PreList),
    {noreply, State#state{animal_list = [Animal | AnimalList], pre_list = List1}};



%% 关闭房间
handle_info(stop, State) ->
    {stop, normal, State};

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



%% 初始化房间
do_init(State) ->
    init_animal(15, State).


%% 批量产生动物
init_animal(0, State) -> State;

%% 新手引导第一步 开始只出1只
init_animal(_N, State = #state{animal_list = List, role_list = RoleList, num = Num, guide_task = 1}) ->
    Animal = #animal_base{base_id = dog, name = "小狗"},
    #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
    NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, xy = XY,  rate = 10},
    push_animal_enter([NewAnimal], RoleList),
    State#state{animal_list = [NewAnimal | List], num = Num + 1};
%% 不出河马
init_animal(N, State = #state{animal_list = List, role_list = RoleList, num = Num, pre_list = PreList, type = Type, guide_task = TaskId = 2}) ->
    {PushList, NewList, NewNum, PreList1} = init_animal(N, [], List, Num, RoleList, PreList, Type, TaskId),
    push_animal_enter(PushList, RoleList),
    NewState = State#state{animal_list = NewList, num = NewNum, pre_list = PreList1},
    NewState;
%% 必出河马 10倍的,掉落1福袋
init_animal(N, State = #state{animal_list = List, role_list = RoleList, num = Num, pre_list = PreList, type = Type, guide_task = TaskId}) when TaskId =< 5->
    case lists:keyfind(hippo, #animal_base.base_id, List) of
        false ->
            Animal = #animal_base{base_id = hippo, name = "河马"},
            #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
            NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, xy = XY,  rate = 100, red_bag = 1, item_list = [{red_bag, 10}]},
            push_animal_enter([NewAnimal], RoleList),
            init_animal(N - 1, State#state{animal_list = [NewAnimal | List], num = Num + 1});
        _ ->
            {PushList, NewList, NewNum, PreList1} = init_animal(N, [], List, Num, RoleList, PreList, Type, TaskId),
            push_animal_enter(PushList, RoleList),
            NewState = State#state{animal_list = NewList, num = NewNum, pre_list = PreList1},
            NewState
    end;
init_animal(N, State = #state{animal_list = List, role_list = RoleList, num = Num, pre_list = PreList, type = Type, guide_task = TaskId}) ->
    case lists:keyfind(hippo, #animal_base.base_id, List) of
            false ->
                Animal = #animal_base{base_id = hippo, name = "河马"},
                #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
                NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, xy = XY,  rate = 1000, red_bag = 1, item_list = []},
                push_animal_enter([NewAnimal], RoleList),
                init_animal(N - 1, State#state{animal_list = [NewAnimal | List], num = Num + 1});
            _ ->
                {PushList, NewList, NewNum, PreList1} = init_animal(N, [], List, Num, RoleList, PreList, Type, TaskId),
                push_animal_enter(PushList, RoleList),
                NewState = State#state{animal_list = NewList, num = NewNum, pre_list = PreList1},
                NewState
    end.


%% 初始化线路
init_animal(0, List1, List, Num, _, PreList, _Type, _) -> {List1, List, Num, PreList};
init_animal(N, List1, List, Num, RoleList, PreList, Type, TaskId) ->
    Animal = #animal_base{base_id = BaseId, rate = [Min, Max]} = get_one_annimal(PreList ++ List, TaskId),
    Rate = sys_rand:rand(Min, Max),
    #animal_route{id = RouteId, time = AllTime, post = Post, xy = XY} = init_animal_route(),
    case lists:member(BaseId, ?animal_pre_notice_list) of
        true ->
            NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = 0, route_id = RouteId, xy = XY, rate = Rate},
            push_pre_animal(BaseId, RoleList),
            erlang:send_after(3000, self(), {add_animal, NewAnimal}),   %% 预警动物之后再加载
            init_animal(N -1, List1, List, Num + 1, RoleList, [NewAnimal | PreList], Type, TaskId);
        _ ->
            NewAnimal = Animal#animal_base{id = Num, end_time = AllTime, post = Post, route_id = RouteId, xy = XY, rate = Rate},
            init_animal(N -1, [NewAnimal | List1], [NewAnimal| List], Num + 1, RoleList,  PreList, Type, TaskId)
    end.



get_one_annimal(List, TaskId) ->
    OnlyList = [Id || #animal_base{base_id = Id} <- List, lists:member(Id, ?animal_only_one_list)],
    NewList = [A||A = #animal_base{base_id = Id} <-get_animal_base(TaskId), not lists:member(Id, OnlyList)],
    case sys_rand:rand_list(NewList, #animal_base.pre) of
        Animal = #animal_base{base_id = type_bomber, bomber_type = List1} ->
            Type = sys_rand:rand_list(List1),
            #animal_base{rate = Rate} = lists:keyfind(Type, #animal_base.base_id, get_animal_base(TaskId)),
            Animal#animal_base{bomber_type = Type, rate = Rate};
        _Animal -> _Animal
    end.


%% 动物是否走出去了
do_animal_out([], NewAnimal, OutAnimal, CreateNum, _) -> {NewAnimal, OutAnimal, CreateNum};
do_animal_out([Animal = #animal_base{route_id = Id, end_time = _End, post = Pos, status = 0} | L], NewAnimal, OutAnimal, CreateNum, TaskId = 1) -> %% 特殊处理
    NewPost = Pos + 6,
    case get_xy(Id, NewPost) of
        {0, 0} ->
            case get_xy(Id, Pos + 1) of
                {0, 0} ->
                    do_animal_out([], NewAnimal, [Animal#animal_base{post = Pos + 1} | OutAnimal], CreateNum + 1,  TaskId);
                XY ->
                    Num = case NewAnimal =:= [] andalso L =:= [] of
                        true -> 1;
                        _ -> 0
                    end,
                    do_animal_out([], [Animal#animal_base{post = Pos + 1, xy = XY} | NewAnimal], OutAnimal, CreateNum + Num, TaskId)
            end;
        XY ->
            do_animal_out([], [Animal#animal_base{post = Pos + 1, xy = XY} | NewAnimal], OutAnimal, CreateNum, TaskId)
    end;
do_animal_out([Animal = #animal_base{route_id = Id, end_time = _End, post = Pos, status = 0, is_horn = Horn} | L], NewAnimal, OutAnimal, CreateNum, TaskId) ->
    case get_xy(Id, Pos + 1) of
        {0, 0} ->
            Num = case Horn of
                0 -> 1;
                _ -> 
                    self() ! {delete_skill, Horn},
                    0
            end,
            do_animal_out(L, NewAnimal, [Animal#animal_base{post = Pos + 1} | OutAnimal], CreateNum + Num, TaskId);
        XY ->
            do_animal_out(L, [Animal#animal_base{post = Pos + 1, xy = XY} | NewAnimal], OutAnimal, CreateNum, TaskId)
    end;
do_animal_out([Animal | L], NewAnimal, OutAnimal, CreateNum, TaskId) ->
    do_animal_out(L, [Animal | NewAnimal], OutAnimal, CreateNum, TaskId).


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



do_hit(Animal = #animal_base{rate = Rate}, AnimalList, Coin, Vip, Luck, Flag, RoleId) ->
    case do_rate(Rate/10, Coin, Flag) of
        true ->
            {HitList, NewAnimalList, ItemList, CreateNum, NewLuck} = do_drop([Animal], AnimalList, Coin, Vip, [], [], 0, Luck, 0, RoleId),
            {HitList, NewAnimalList, 0, ItemList, CreateNum, NewLuck};
        _ ->
            false
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



%% 计算所获得的金币和红包
calc_coin_red(_, Rate, Coin, 0) ->
    Win = trunc(Rate * Coin/10),
    [#p_assets{type = coin, num = Win}];
calc_coin_red(_, Rate, Coin, 1) when Rate >= 500 ->
    N = 20,
    Win = trunc(Rate * Coin/10),
    RedBag = min(2000, trunc(N * Win/10000)),
    Coin1 = Win - trunc(RedBag * 1.2 * 100),
    [#p_assets{type = red_bag, num = RedBag}, #p_assets{type = coin, num = Coin1}];
calc_coin_red(_, Rate, Coin, _) ->
    Win = trunc(Rate * Coin/10),
    [#p_assets{type = coin, num = Win}].


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
do_drop_item(Animal = #animal_base{base_id = BaseId, rate = Rate, item_list = List, red_bag = RedBag}, _ItemList, Coin, _Vip, Luck, _Active, _RoleId)->
    Coins  = calc_coin_red(BaseId, Rate, Coin, RedBag),
    DropList = Coins ++ [#p_assets{type = _Type, num = _Num}||{_Type, _Num}<-List],
    NewItemList = do_sort_item(DropList, []),
    {Animal#animal_base{drop_list = DropList}, NewItemList, Luck}.



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
get_animal_base(TaskId) when TaskId =< 5->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 2000}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 1500}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 1500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 1500}
        ,#animal_base{base_id = horse,   name = "马", rate =   [60, 60],       pre = 1500}
    ];
%% 动物基本配置
get_animal_base(_) ->
    [
        #animal_base{base_id = turtle,   name = "乌龟", rate = [8, 15],        pre = 2000}
        ,#animal_base{base_id = cock,    name = "小鸡", rate = [12, 12],       pre = 1500}
        ,#animal_base{base_id = dog,     name = "小狗", rate = [20, 20],       pre = 1500}
        ,#animal_base{base_id = monkey,  name = "猴子", rate = [40, 40],       pre = 1500}
        ,#animal_base{base_id = horse,   name = "马", rate = [60, 60],         pre = 1500}
        ,#animal_base{base_id = ox,      name = "奶牛", rate = [100, 100],     pre = 1000}
        ,#animal_base{base_id = panda,   name = "熊猫", rate = [200, 200],     pre = 800, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
        ,#animal_base{base_id  = hippo,  name = "河马", rate = [1000, 1000],   pre = 500, is_notice = 1, red_bag = 1, item_list = [{ice, 1, 500}, {locking, 1, 500}, {gold, 2, 200}]}
    ].

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

