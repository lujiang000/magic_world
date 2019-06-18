%%----------------------------------------------------
%% 人物农场管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(farm).
-behaviour(gen_server).
-export([start_link/1
        ,open_farm/2
        ,enter/3
        ,get_info/1
        ,buy_animal/3
        ,get_reward/1
        ,get_pw_log/2
        ,out/1
        ,get_vip_animal/1
        ,to_animal_type_integer/1
        ,get_animal_log/1
        ,get_buy_log/3
        ,get_death_log/3
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("farm.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").
-include("animal.hrl").
-include("role.hrl").
-include("rank.hrl").


%% 解锁房间类型
open_farm(Pid, Type) ->
    case catch gen_server:call(Pid, {open_farm, Type}) of
        ok -> ok;
        _ -> {false, ?error_busy}
    end.


%% 获取日志统计
get_animal_log(Id) ->
    case ets:lookup(farm_log, Id) of
        [#farm_log{list = List}] -> List;
        _ -> []
    end.

%% 获取购买日志
get_buy_log(RoleID, Type, Id) ->
    Sql = "select animal_num, time from farm_buy_tail_log where role_id = ? and farm_type = ? and animal_type  = ? order by time desc limit 100",
    case db:get_all(Sql, [RoleID, Type, to_animal_type_integer(Id)]) of
        {ok, List} ->
            [#p_buy_log{base_id = Id, buy_num = Num, time = Time} || [Num, Time]<- List];
        _ ->
            []
    end.

%% 获取死亡日志
get_death_log(RoleID, Type, Id) ->
    Sql = "select hit_role_name, time from farm_death_log where role_id = ? and farm_type = ? and animal_type  = ? order by time desc limit 100",
    case db:get_all(Sql, [RoleID, Type, to_animal_type_integer(Id)]) of
        {ok, List} ->
            [#p_death_log{base_id = Id, name = Name, time = Time} || [Name, Time]<- List];
        _ ->
            []
    end.


%% 牧场主离线
out(_Role = #role{farm_pid = Pid}) when is_pid(Pid)->
    Pid ! out;
out(_) -> ok.

%% 领取收益
get_reward(Role = #role{role_id = RoleID, farm_pid = Pid, farm_reward_log = List}) when is_pid(Pid)->
    case catch gen_server:call(Pid, get_reward) of
        {ok, N} -> 
            {ok, NewRole} = role_lib:do_add(Role, [{candy, N}]),
            Now = date:unixtime(),
            log_db:log(farm_reward_log, insert, [RoleID, N, Now]),
            NewList = lists:sublist([#p_farm_reward_log{num = N, time = Now} | List], 20),
            {ok, #p_assets{type = candy, num = N}, NewRole#role{farm_reward_log = NewList}};
        _ -> {false, ?error_busy}
    end;
get_reward(_) -> {false, ?error_act}.

%% 获取每日营收日志
get_pw_log(_Role = #role{farm_pid = Pid, role_id = RoleID}, Type) when is_pid(Pid)->
    case db:get_all("select cost, win, time from farm_pw_log where role_id = ? and farm_type = ? order by time desc limit 20", [RoleID, Type]) of
        {ok, List} ->
            NewList = [#p_farm_pw_log{put = Cost, out = Win, time = Time}||[Cost, Win, Time] <-List],
            {ok, NewList};
        _ ->
            {false, ?error_busy}
    end;
get_pw_log(_, _) -> {false, ?error_act}.

    

%% 获取自己牧场信息
get_info(undefined) ->
    {ok, #m_2101_toc{level = 0, exp = 0, win = 0, list = []}};
get_info(Pid) ->
    case catch gen_server:call(Pid, get_info) of
        {ok, Data} -> {ok, Data};
        {false, Reason} -> {false, Reason};
        _ -> {false, ?error_busy}
    end.


%% 进入牧场
enter(Role, Pid, Type) ->
    case catch gen_server:call(Pid, {get_room_pid, Type}) of
        {ok, Pid1} -> 
            case catch gen_server:call(Pid1, {enter, role_conver:to_animal_role(Role)}) of
                {ok, Data} ->
                    {ok, Data, Role#role{room_type = Type, room_pid = Pid1, status = ?status_farm}};
                _Err ->
                    {false, ?error_busy}
            end;
        {false, Reason} -> 
            {false, Reason};
        _ ->
            {false, ?error_busy}
    end.

%% 购买牧场动物
buy_animal(Role = #role{farm_pid = Pid, role_id = RoleId}, Type, List) ->
    BaseList = farm_animal:get_animal_base(),
    Coin = farm_animal:get_type_coin(Type),
    case calc_coin(List, 0, BaseList, Coin) of
        {ok, Cost} ->
            case role_lib:do_cost_coin(Role, Cost) of
                {ok, NewRole} ->
                    Time = date:unixtime(),
                    do_buy_log(List, BaseList, Coin, Type, Time, RoleId),
                    Pid ! {buy_animal, Type, List, util:ceil(Cost/100)},
                    {ok, Cost, NewRole};
                {false, Reason} ->
                    {false, Reason}
            end;
        {false, Reason} ->
            {false, Reason}
    end.

%% 购买详情日志
do_buy_log([#p_farm_animal{type = BaseId, num = Num} | L], List, Coin, FarmType, Time, RoleId) ->
    case lists:keyfind(BaseId, #animal_base.base_id, List) of
        #animal_base{rate = Rate} ->
            Add = util:ceil(Rate * Coin * Num),
            log_db:log(farm_buy_tail_log, insert, [RoleId, FarmType, to_animal_type_integer(BaseId), Num, Add, Time]);
        _ ->
            ok
    end,
    do_buy_log(L, List, Coin, FarmType, Time, RoleId);
do_buy_log([], _, _, _, _, _) -> ok.

%% 动物类型转换
to_animal_type_integer(turtle) -> 1;
to_animal_type_integer(cock) -> 2;
to_animal_type_integer(dog) -> 3;
to_animal_type_integer(monkey) -> 4;
to_animal_type_integer(horse) -> 5;
to_animal_type_integer(ox) -> 6;
to_animal_type_integer(panda) -> 7;
to_animal_type_integer(hippo) -> 8;
to_animal_type_integer(lion) -> 9;
to_animal_type_integer(elephant) -> 10;
to_animal_type_integer(pikachu) -> 11;
to_animal_type_integer(bomber) -> 12;
to_animal_type_integer(tiger) -> 13;
to_animal_type_integer(sheep) -> 14;
to_animal_type_integer(bear) -> 15;
to_animal_type_integer(type_bomber) -> 16;
to_animal_type_integer(xsx) -> 17;
to_animal_type_integer(dsy) -> 18;
to_animal_type_integer(area_bomber) -> 19;
to_animal_type_integer(self_elephant) -> 20;
to_animal_type_integer(gold_pick) -> 21.



%% 计算消耗多少金币
calc_coin([#p_farm_animal{type = BaseId, num = Num} | L], All, List, Coin) -> 
    case lists:keyfind(BaseId, #animal_base.base_id, List) of
        #animal_base{rate = Rate} ->
            Add = util:ceil(Rate * Coin * Num),
            calc_coin(L, All + Add, List, Coin);
        _ ->
            {false, ?error_act}
    end;
calc_coin([], All, _, _) -> {ok, All}.

    



start_link(Farm) ->
    gen_server:start_link(?MODULE, [Farm], []).


init([Farm = #farm{}]) ->
    process_flag(trap_exit, true),
    rank:handle(?rank_farm, Farm),
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), save_log),
    save_ets(Farm#farm{farm_pid = self()}),
    {ok, Farm#farm{farm_pid = self()}}.


%% 获取自己信息
handle_call(get_info, _From, State = #farm{level = Level, exp = Exp, profit = Win, farm_list = List}) ->
    Data = #m_2101_toc{level = Level, exp = Exp, win = Win, list = to_farm_info(List)},
    {reply, {ok, Data}, State};

%% 领取收益
handle_call(get_reward, _From, State = #farm{profit = Win}) ->
    N = erlang:trunc(Win/40000),
    Left = Win - N * 40000,
    {reply, {ok, N}, State#farm{profit = Left}};

%% 解锁房间类型
handle_call({open_farm, Type}, _From, State = #farm{farm_list = List}) ->
    case lists:keyfind(Type, #farm_info.type, List) of
        Farm = #farm_info{max_room = Num} -> 
            NewList = lists:keyreplace(Type, #farm_info.type, List, Farm#farm_info{max_room = Num + 1}),
            NewState = State#farm{farm_list = NewList},
            save_ets(NewState),
            {reply, ok, NewState};
        _ ->
            NewState = State#farm{farm_list = [#farm_info{type = Type} | List]},
            save_ets(NewState),
            {reply, ok, NewState}
    end;

%% 进入房间
handle_call({get_room_pid, Type}, _From, State = #farm{farm_list = FarmList}) ->
    case lists:keyfind(Type, #farm_info.type, FarmList) of
        #farm_info{max_room = Max} -> 
            get_room_pid(State, Type, Max);
        _ ->
            {reply, {false, ?error_act}, State}
    end;

%% 请求产出动物
handle_call({create_animal, Num, Type, P, W, DeathList}, _From, State = #farm{farm_list = FarmList, profit = Win}) ->
    case lists:keyfind(Type, #farm_info.type, FarmList) of
        Farm = #farm_info{animal_list = List, p = P1, w = W1} -> 
            {NewDeathList, NewList, Animals} = do_create1(Num, DeathList, List, []),
            NewFarmList = lists:keyreplace(Type, #farm_info.type, FarmList, Farm#farm_info{animal_list = NewList, p = P1 + P, w = W1 + W}),
            NewState = State#farm{farm_list = NewFarmList, profit = Win + P},
            save_ets(NewState),
            {reply, {ok, Animals, NewDeathList}, NewState};
        _ -> 
            {reply, {ok, []}, State}
    end;


handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 购买动物
handle_info({buy_animal, Type, List, Add}, State = #farm{role_id = RoleId, farm_list = FarmList, level = Lev, exp = Exp}) ->
    case lists:keyfind(Type, #farm_info.type, FarmList) of
        Farm = #farm_info{animal_list = AnimalList} ->
            NewAnimalList = do_add_animal(AnimalList, List),
            NewFarmList = lists:keyreplace(Type, #farm_info.type, FarmList, Farm#farm_info{animal_list = NewAnimalList}),
            {NewLevel, NewExp} = do_level_up(Lev, Exp + Add),
            rank:handle(?rank_farm, State#farm{level = NewLevel, exp = NewExp}),
            save_ets(State#farm{farm_list = NewFarmList, level = NewLevel, exp = NewExp}),
            save_buy_log({RoleId, Type}, List),
            {noreply, State#farm{farm_list = NewFarmList, level = NewLevel, exp = NewExp}};
        _ ->
            {noreply, State}
    end;



%% 房间关闭回收宠物
handle_info({close, Type, Id, Animals, P, W}, State = #farm{farm_list = FarmList, room_list = RoomList1, profit = Win}) ->
    NewFarmList = case lists:keyfind(Type, #farm_info.type, FarmList) of
        Farm = #farm_info{animal_list = List, p = P1, w = W1} ->
            NewList = do_recover(List, Animals),
            lists:keyreplace(Type, #farm_info.type, FarmList, Farm#farm_info{animal_list = NewList, p = P1 + P, w = W1 + W});
        _ ->
            FarmList
    end,
    NewRoomList = case lists:keyfind(Type, 1, RoomList1) of
        {Type, RoomList} ->
            case lists:keyfind(Id, #animal_room.id, RoomList) of
                #animal_room{}->            
                    NewList1 = lists:keydelete(Id, #animal_room.id, RoomList),
                    lists:keyreplace(Type, 1, RoomList1, {Type, NewList1});
                _ ->
                    RoomList1
            end;
        _ ->
            RoomList1
    end,
    NewState = State#farm{farm_list = NewFarmList, room_list = NewRoomList, profit = Win + P},
    save_ets(NewState),
    {noreply, NewState};

%% 房间人数减少通知
handle_info({delete_room_num, Id, Type}, State = #farm{room_list = List}) ->
    case lists:keyfind(Type, 1, List) of
        {Type, RoomList} ->
            case lists:keyfind(Id, #animal_room.id, RoomList) of
                #animal_room{num = Num, pid = Pid} when Num =< 1->             %% 房间没有人了 房间进程结束
                    NewList = lists:keydelete(Id, #animal_room.id, RoomList),
                    NewList1 = lists:keyreplace(Type, 1, List, {Type, NewList}),
                    Pid ! stop,
                    erlang:send_after(5000, self(), check_status),
                    {noreply, State#farm{room_list = NewList1}};
                Room = #animal_room{num = Num} ->
                    NewList = lists:keyreplace(Id, #animal_room.id, RoomList, Room#animal_room{num = Num - 1}),
                    NewList1 = lists:keyreplace(Type, 1, List, {Type, NewList}),
                    {noreply, State#farm{room_list = NewList1}};
                _ ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;



%% 日志存贮
handle_info(save_log, State = #farm{role_id = RoleID, farm_list = FarmList}) ->
    Now = date:unixtime(),
    NewFarmList = do_save_log(RoleID, FarmList, [], Now),
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), save_log),
    save_dets(State#farm{farm_list = NewFarmList}),
    {noreply, State#farm{farm_list = NewFarmList}};

%% 动物死亡日志
handle_info({death_log, {RoleId, Type, HitList}}, State) ->
    NewList = to_log_list(HitList, []),
    save_death_log({RoleId, Type}, NewList),
    {noreply, State};



%% 赠送Vip福利
handle_info({send, Vip}, State = #farm{farm_list = FarmList}) ->
    case lists:keyfind(1, #farm_info.type, FarmList) of
        Farm = #farm_info{animal_list = AnimalList} ->
            List = get_vip_animal(Vip),
            NewAnimalList = do_add_animal(AnimalList, List),
            NewFarmList = lists:keyreplace(1, #farm_info.type, FarmList, Farm#farm_info{animal_list = NewAnimalList}),
            save_ets(State#farm{farm_list = NewFarmList}),
            {noreply, State#farm{farm_list = NewFarmList}};
        _ ->
            {noreply, State}
    end;


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, Farm) ->
    catch save_ets(Farm),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_add_animal(List, [#p_farm_animal{type = Type, num = Num} | L]) -> 
    NewList = case lists:keyfind(Type, #p_farm_animal.type, List) of
        Animal = #p_farm_animal{num = Num1} ->
            lists:keyreplace(Type, #p_farm_animal.type, List, Animal#p_farm_animal{num = Num + Num1});
        _ ->
            [#p_farm_animal{type = Type, num = Num} | List]
    end,
    do_add_animal(NewList, L);
do_add_animal(List, []) -> List.


%% 投入产出日志
do_save_log(RoleID, [Farm = #farm_info{type = Type, p = P, w = W} | L], List, Time) -> 
    log_db:log(farm_pw_log, insert, [RoleID, Type, W, P, Time]),
    do_save_log(RoleID, L, [Farm#farm_info{p = 0, w = 0} | List], Time);
do_save_log(_, [], List, _) -> List.


%% vip赠送原则
get_vip_animal(1) ->
    [#p_farm_animal{type = cock, num = 8}];
get_vip_animal(2) ->
    [#p_farm_animal{type = dog, num = 8}];
get_vip_animal(3) ->
    [#p_farm_animal{type = monkey, num = 8}];
get_vip_animal(4) ->
    [#p_farm_animal{type = horse, num = 8}];
get_vip_animal(5) ->
    [#p_farm_animal{type = ox, num = 10}];
get_vip_animal(6) ->
    [#p_farm_animal{type = panda, num = 10}];
get_vip_animal(7) ->
    [#p_farm_animal{type = xsx, num = 10}];
get_vip_animal(8) ->
    [#p_farm_animal{type = pikachu, num = 10}];
get_vip_animal(9) ->
    [#p_farm_animal{type = dsy, num = 10}];
get_vip_animal(10) ->
    [#p_farm_animal{type = hippo, num = 10}];
get_vip_animal(11) ->
    [#p_farm_animal{type = lion, num = 10}];
get_vip_animal(12) ->
    [#p_farm_animal{type = elephant, num = 10}];
get_vip_animal(13) ->
    [#p_farm_animal{type = hippo, num = 10}, #p_farm_animal{type = elephant, num = 10}];
get_vip_animal(14) ->
    [#p_farm_animal{type = lion, num = 10}, #p_farm_animal{type = elephant, num = 10}];
get_vip_animal(15) ->
    [#p_farm_animal{type = hippo, num = 10}, #p_farm_animal{type = lion, num = 10}, #p_farm_animal{type = elephant, num = 10}];
get_vip_animal(_) ->
    [].


%%%% 产出动物类型列表
%%do_create(_Num, [], List) -> {[], List};
%%do_create(0, List1, List) -> {List1, List};
%%do_create(Num, List1, List) ->
%%    Animal = #p_farm_animal{type = Id, num = Num1} = sys_rand:rand_list(List1, #p_farm_animal.num),
%%    NewNum = Num1 -1,
%%    NewList1 = case NewNum of
%%        0 -> 
%%            lists:keydelete(Id, #p_farm_animal.type, List1);
%%        _ -> 
%%            lists:keyreplace(Id, #p_farm_animal.type, List1, Animal#p_farm_animal{num = NewNum})
%%    end,
%%    do_create(Num - 1, NewList1, [Id | List]).

%% 根据死亡动物类型列表产生新的动物
do_create1(_, DeathList, [], List) -> {DeathList, [], List};
do_create1(0, [], List1, List) -> {[], List1, List};
do_create1(CreateNum, [], List1, List) -> 
    Animal = #p_farm_animal{type = Id, num = Num1} = sys_rand:rand_list(List1, #p_farm_animal.num),
    NewNum = Num1 -1,
    NewList1 = case NewNum of
        0 -> 
            lists:keydelete(Id, #p_farm_animal.type, List1);
        _ -> 
            lists:keyreplace(Id, #p_farm_animal.type, List1, Animal#p_farm_animal{num = NewNum})
    end,
    do_create1(CreateNum - 1, [], NewList1, [Id | List]);
do_create1(CreateNum, [Id | L], List1, List) ->
    {NewId, NewList1} = case lists:keyfind(Id, #p_farm_animal.type, List1) of
        #p_farm_animal{num = 1} ->
            {Id, lists:keydelete(Id, #p_farm_animal.type, List1)};
        Animal = #p_farm_animal{num = Num} when Num > 1->
            {Id, lists:keyreplace(Id, #p_farm_animal.type, List1, Animal#p_farm_animal{num = Num - 1})};
        _ ->
            Animal = #p_farm_animal{type = Id1, num = Num1} = sys_rand:rand_list(List1, #p_farm_animal.num),
            NewNum = Num1 -1,
            NewList0 = case NewNum of
                0 -> 
                    lists:keydelete(Id1, #p_farm_animal.type, List1);
                _ -> 
                    lists:keyreplace(Id1, #p_farm_animal.type, List1, Animal#p_farm_animal{num = NewNum})
            end,
            {Id1, NewList0}
    end,
    do_create1(CreateNum - 1, L, NewList1, [NewId | List]).

%% 回收动物
do_recover(List, []) -> List;
do_recover(List, [Id | L]) ->
    NewList = case lists:keyfind(Id, #p_farm_animal.type, List) of
        Animal = #p_farm_animal{num = Num} ->
            lists:keyreplace(Id, #p_farm_animal.type, List, Animal#p_farm_animal{num = Num + 1});
        _ ->
            [#p_farm_animal{type = Id, num = 1} | List]
    end,
    do_recover(NewList, L).

%% 保存ets
save_ets(Farm) ->
    ets:insert(farm, Farm#farm{room_list = []}).

%% 保存dets
save_dets(Farm) ->
    dets:insert(farm, Farm#farm{room_list = []}).


%% 转换前端数据
to_farm_info(List)->
    [#p_farm_info{type = Type, put = W, out = P, list = Animals, max_room = Max}|| #farm_info{type = Type, p = P, w = W, animal_list = Animals, max_room = Max}<-List].

%% 获取房间的pid
get_room_pid(State = #farm{id = NextId, room_list = List}, Type, Max) ->
    case lists:keyfind(Type, 1, List) of
        {Type, RoomList} ->
            case find_room(RoomList) of
                Room = #animal_room{id = Id, pid = Pid, num = Num} ->
                    NewList = lists:keydelete(Id, #animal_room.id, RoomList),
                    NewList1 = [Room#animal_room{num = Num + 1} | NewList],
                    {reply, {ok, Pid}, State#farm{room_list = lists:keyreplace(Type, 1, List, {Type, NewList1})}};
                _ ->
                    case erlang:length(RoomList) >= Max of
                        true -> 
                            {reply, {false, ?error_farm_max}, State};
                        _ ->
                            case start_room(State, Type) of
                                {Room = #animal_room{pid = Pid}, NewState} ->
                                    NewList = [Room | RoomList],
                                    {reply, {ok, Pid}, NewState#farm{id = NextId + 1, room_list = lists:keyreplace(Type, 1, List, {Type, NewList})}};
                                Reason ->
                                    {reply, Reason, State}
                            end
                    end
            end;
        _ ->
            case start_room(State, Type) of
                {Room = #animal_room{pid = Pid}, NewState} ->
                    {reply, {ok, Pid}, NewState#farm{id = NextId + 1, room_list = [{Type, [Room]} | List]}};
                Reason ->
                    {reply, Reason, State}
            end
    end.


%% 找房间有空位置的房间
find_room([]) -> false; 
find_room([Room = #animal_room{num = Num}| _L]) when Num < 5->
    Room;
find_room([_Room | L]) ->
    find_room(L).

-define(default_animals, [elephant, lion, hippo, pikachu, dsy, xsx, panda, ox, horse, monkey, dog, cock]).

%% 新开一个房间
start_room(State = #farm{id = NextId, farm_list = FarmList, role_id = RoleID, name = Name}, Type) ->
    case lists:keyfind(Type, #farm_info.type, FarmList) of
        Farm = #farm_info{animal_list = List} -> 
            List1 = [Animal|| Animal = #p_farm_animal{num = Num1}<-List, Num1 > 0],
            case List1 =/= []of
                true ->
                    {_Death,  NewList, Animals} = do_create1(15, ?default_animals, List1, []),
                    NewFarmList = lists:keyreplace(Type, #farm_info.type, FarmList, Farm#farm_info{animal_list = NewList}),
                    case catch farm_animal:start_link(Type, NextId, Animals, self(), RoleID, Name) of
                        {ok, Pid} ->
                            Room = #animal_room{id = NextId, type = Type, num = 1, pid = Pid},
                            {Room, State#farm{farm_list = NewFarmList}};
                        _Err ->
                            false
                    end;
                _ ->
                    {false, ?error_farm_animal}
            end;
        _ -> 
            false
    end.


do_level_up(Lev, Exp) ->
    Need = get_level_exp(Lev + 1),
    case Exp >= Need of
        true ->
            do_level_up(Lev + 1, Exp - Need);
        _ ->
            {Lev, Exp}
    end.

get_level_exp(2) -> 500000;
get_level_exp(3) -> 1000000;
get_level_exp(4) -> 2000000;
get_level_exp(5) -> 5000000;
get_level_exp(6) -> 10000000;
get_level_exp(7) -> 30000000;
get_level_exp(8) -> 50000000;
get_level_exp(9) -> 100000000;
get_level_exp(_) -> 500000000.


%% 购买日志
save_buy_log(Id = {RoleID, Type}, List) ->
    case ets:lookup(farm_log, Id) of
        [Log = #farm_log{list = OldList}] ->
            List1 = save_buy_log1(OldList, List),
            ets:insert(farm_log, Log#farm_log{list = List1});
        _ ->
            List1 = [#p_animal_log{base_id = Type1, buy_num = Num, death_num = 0}||#p_farm_animal{type = Type1, num = Num} <-List],
            ets:insert(farm_log, #farm_log{id = Id, role_id = RoleID, type = Type, list = List1})
    end.
save_buy_log1(List, [#p_farm_animal{type = Type, num = Num} | L]) -> 
    NewList = case lists:keyfind(Type, #p_animal_log.base_id, List) of
        Log = #p_animal_log{buy_num = OldNum} ->
            lists:keyreplace(Type, #p_animal_log.base_id, List, Log#p_animal_log{buy_num = OldNum + Num});
        _ ->
            [#p_animal_log{base_id = Type, buy_num = Num, death_num = 0} | List]
    end,
    save_buy_log1(NewList, L);
save_buy_log1(List, []) -> List.


%% 动物死亡日志
save_death_log(Id = {RoleID, Type}, List) ->
    case ets:lookup(farm_log, Id) of
        [Log = #farm_log{list = OldList}] ->
            List1 = save_death_log1(OldList, List),
            ets:insert(farm_log, Log#farm_log{list = List1});
        _ ->
            List1 = [#p_animal_log{base_id = Type1, buy_num = 0, death_num = Num}|| {Type1, Num} <-List],
            ets:insert(farm_log, #farm_log{id = Id, role_id = RoleID, type = Type, list = List1})
    end.

save_death_log1(List, [{Type, Num} | L]) -> 
    NewList = case lists:keyfind(Type, #p_animal_log.base_id, List) of
        Log = #p_animal_log{death_num = OldNum} ->
            lists:keyreplace(Type, #p_animal_log.base_id, List, Log#p_animal_log{death_num = OldNum + Num});
        _ ->
            [#p_animal_log{base_id = Type, buy_num = 0, death_num = Num} | List]
    end,
    save_death_log1(NewList, L);
save_death_log1(List, []) -> List.

%% 死亡动物列表转换
to_log_list([#animal_base{base_id = Id} | L], List) -> 
    NewList = case lists:keyfind(Id, 1, List) of
        {Id, Num} ->
            lists:keyreplace(Id, 1, List, {Id, Num + 1});
        _ ->
            [{Id, 1} | List]
    end,
    to_log_list(L, NewList);
to_log_list([], List) -> List.
