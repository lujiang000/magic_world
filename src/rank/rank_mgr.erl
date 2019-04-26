%%----------------------------------------------------
%% 排行榜管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(rank_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,lookup/1
        ,clean/1
        ,lookup/2
        ,get_rank_info/5
        ,lookup_list/1
        ,save_and_clean/2
        ,web_look/2
        ,get_old_rank_info/6
        ,do_daily_kill_reward/1
        ,do_week_kill_reward/1
        ,do_daily_great_reward/1
        ,do_week_great_reward/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").
-include("rank.hrl").
-include("role.hrl").
-include("all_pb.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 查询ets表
-spec lookup(pos_integer()) -> #rank{}.
lookup(Type) ->
    case catch ets:lookup(rank, Type) of
        [Rank = #rank{}] -> Rank;
        _ -> #rank{type = Type}
    end.

%% 获取前几的排行详细信息
lookup(Type, N) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List}] -> 
            lists:sublist(List, N);
        _ -> 
            []
    end.

%% 全部排行的列表 
lookup_list(Type) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List}] -> 
            List;
        _ -> 
            []
    end.

%% 获取排行详细信息，向下翻
get_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 0) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List, len = Len}] when Start < Len + 1-> 
            NewList = lists:sublist(List, Start + 1, Num),
            NewList1 = to_p_rank_info(NewList, Start + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        [#rank{list = List}] ->
            {Index, Value} = find_index(RoleId, List),
            {[], Index, Value};
        _ -> 
            {[], 0, 0}
    end;
%% 向上翻
get_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 1) ->
    {NewStart, NewNum} = case Start - 1 - Num >= 0 of
        true ->
            {Start -1 -Num, Num};
        _ ->
            {0, Start -1}
    end,
    case catch ets:lookup(rank, Type) of
        [#rank{list = List, len = Len}] when Start =< Len -> 
            NewList = lists:sublist(List, NewStart + 1, NewNum),
            NewList1 = to_p_rank_info(NewList, NewStart + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        _ -> 
            {[], 0, 0}
    end.

%% 获取以前排行详细信息，向下翻
get_old_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 0, Date) ->
    Time = date:unixtime(zero) - 86400 * Date,
    case catch ets:match_object(rank_log, #rank_log{type = Type, time = Time, _ = '_'}) of
        [#rank_log{list = List, len = Len}] when Start < Len + 1-> 
            NewList = lists:sublist(List, Start + 1, Num),
            NewList1 = to_p_rank_info(NewList, Start + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        [#rank_log{list = List}] ->
            {Index, Value} = find_index(RoleId, List),
            {[], Index, Value};
        _ -> 
            {[], 0, 0}
    end;
%% 向上翻
get_old_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 1, Date) ->
    {NewStart, NewNum} = case Start - 1 - Num >= 0 of
        true ->
            {Start -1 -Num, Num};
        _ ->
            {0, Start -1}
    end,
    Time = date:unixtime(zero) - 86400 * Date,
    case catch ets:match_object(rank_log, #rank_log{type = Type, time = Time, _ = '_'}) of
        [#rank_log{list = List, len = Len}] when Start =< Len -> 
            NewList = lists:sublist(List, NewStart + 1, NewNum),
            NewList1 = to_p_rank_info(NewList, NewStart + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        _ -> 
            {[], 0, 0}
    end.

%% 查找排行榜多少名
find_index(RoleId, List) ->
    case lists:keyfind(RoleId, #rank_role.role_id, List) of
        #rank_role{} ->
            find_index(RoleId, List, 1);
        _ ->
            {0, 0}
    end.
find_index(_RoleId, [], _N) -> {0, 0};
find_index(RoleId, [#rank_role{role_id = RoleId, value1 = Value} | _], N) -> {N, Value};
find_index(RoleId, [_ | L], N) -> 
    find_index(RoleId, L, N + 1).


%% 转换前端数据并且添加排名
to_p_rank_info([], _, List) -> List;
to_p_rank_info([#rank_role{id = Id, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = V1, sign = Sign} | L], Num, List) ->
    Data = #p_rank_info{num = Num, id = Id, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = V1, sign = Sign},
    to_p_rank_info(L, Num + 1, [Data | List]).


%% 清空一个排行
clean(Type) ->
    case catch ets:lookup(rank, Type) of
        [#rank{}] -> 
            ets:insert(rank, #rank{type = Type});
        _ -> 
            ok
    end.

%% 保存并且清空排行
save_and_clean(Type, Time) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List, len = Len}] -> 
            ets:insert(rank_log, #rank_log{type = Type, list = List, time = Time, len = Len}),
            ets:insert(rank, #rank{type = Type});
        _ -> 
            ok
    end.

%% 后台查看排行榜信息
web_look(Type, Time) ->
    Zero = date:unixtime(zero),
    case Zero =:= Time of
        true -> 
            case catch ets:lookup(rank, Type) of
                [#rank{list = List}] -> 
                    [[{role_id, RoleId}, {name, Name}, {value, Value}]|| #rank_role{role_id = RoleId, name = Name, value1 = Value}<-List];
                _ -> 
                    []
            end;
        _ -> 
            case catch ets:match_object(rank_log, #rank_log{type = Type, time = Time, _ = '_'}) of
                [#rank_log{list = List}] -> 
                    [[{role_id, RoleId}, {name, Name}, {value, Value}]|| #rank_role{role_id = RoleId, name = Name, value1 = Value}<-List];
                _ -> 
                    []
            end
    end.


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    State = #state{},
    erlang:send_after(date:next_diff(23, 0, 0) * 1000, self(), stop_calc),
    erlang:send_after(date:next_diff(23, 59, 59) * 1000, self(), reward),
    ets:new(rank, [set, named_table, public, {read_concurrency, true}, {keypos, #rank.type}]),
    dets:open_file(rank, [{file, "./dets/rank.dets"},  {keypos, #rank.type}, {type, set}]),
    ets:new(rank_log, [duplicate_bag, named_table, public, {read_concurrency, true}, {keypos, #rank_log.type}]),
    dets:open_file(rank_log, [{file, "./dets/rank_log.dets"},  {keypos, #rank_log.type}, {type, duplicate_bag}]),
    ets:from_dets(rank, rank),
    ets:from_dets(rank_log, rank_log),
    self() ! init,
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    start_rank_zone(5),
    {noreply, State};

%% 停止数据计算
handle_info(stop_calc, State) ->
    erlang:send_after(date:next_diff(23, 0, 0) * 1000, self(), stop_calc),
    do_stop_calc(?rank_reward_list),
    {noreply, State};

%% 发完奖励重新开始数据计算
handle_info(reward, State) ->
    Time = date:unixtime(zero),
    do_reward(?rank_reward_list, Time),
    erlang:send_after(date:next_diff(23, 59, 59) * 1000, self(), reward),
    timer:sleep(1000),
    do_start_calc(?rank_reward_list),
    {noreply, State};

%% 目前没有需求保存的暂时不做保存
handle_info({save, Type, _Time}, State) ->
    clean(Type),
    {noreply, State};

handle_info({'EXIT', Pid, _}, State = #state{}) ->
    ?ERR("Pid:~p", [Pid]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("排行进程关闭......"),
    ets:to_dets(rank, rank),
    ets:to_dets(rank_log, rank_log),
    ?INFO("排行进程关闭完成"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% 启动排行榜运算进程
start_rank_zone(0) -> ok;
start_rank_zone(N) ->
    rank_zone:start_link(N),
    start_rank_zone(N - 1).


%% 通知计算进程停止接收数据
do_stop_calc([]) -> ok;
do_stop_calc([Type = ?rank_kill_week | L]) ->
    #rank_config{zone = Zone} = rank:get_rank_config(Type),
    case date:day_of_the_week(date:unixtime()) of
        2 -> 
            Zone ! stop_calc,
            ok;
        4 ->
            Zone ! stop_calc,
            ok;
        6 -> 
            Zone ! stop_calc,
            ok;
        _ -> ok
    end,
    do_stop_calc(L);
do_stop_calc([Type | L]) ->
    #rank_config{zone = Zone} = rank:get_rank_config(Type),
    Zone ! stop_calc,
    do_stop_calc(L).

%% 通知计算进程开始接收数据
do_start_calc([]) -> ok;
do_start_calc([Type | L]) ->
    #rank_config{zone = Zone} = rank:get_rank_config(Type),
    Zone ! start_calc,
    do_start_calc(L).

%% 发奖励
do_reward([], _) -> ok;
do_reward([?rank_kill | L], Time) ->
    do_daily_kill_reward(),
    save_and_clean(?rank_kill, Time),
    do_reward(L, Time);
do_reward([?rank_great_match | L], Time) ->
    do_daily_great_reward(),
    save_and_clean(?rank_great_match, Time),
    do_reward(L, Time);
do_reward([?rank_week_great_match | L], Time) ->
    case date:day_of_the_week(Time) of
        7 ->
            do_week_great_reward(),
            save_and_clean(?rank_week_great_match, Time);
        _ ->
            ok
    end,
    do_reward(L, Time);
do_reward([?rank_kill_week | L], Time) ->
    case date:day_of_the_week(Time) of
        2 ->
            do_week_kill_reward(),
            save_and_clean(?rank_kill_week, Time);
        4 ->
            do_week_kill_reward(),
            save_and_clean(?rank_kill_week, Time);
        6 ->
            do_week_kill_reward(),
            save_and_clean(?rank_kill_week, Time);
        _ ->
            ok
    end,
    do_reward(L, Time);
do_reward([_Type | L], Time) ->
    do_reward(L, Time).


%% 每日击杀榜发奖励
do_daily_kill_reward() ->
    List = lookup_list(?rank_kill),
    Time = date:unixtime(),
    case setting_mgr:get(?setting_kill_rank) of
        {ok, RewardList} when RewardList =/= [] -> 
            RewardList1 = [{N, mail_mgr:to_atom_assets(Type), Num}||[N, Type, Num] <-RewardList],
            do_daily_kill_reward(List, 1, RewardList1, Time);
        _ ->
            do_daily_kill_reward(List, 1, Time)
    end.

do_daily_kill_reward(Time) ->
    List = lookup_list(?rank_kill),
    case setting_mgr:get(?setting_kill_rank) of
        {ok, RewardList} when RewardList =/= [] -> 
            RewardList1 = [{N, mail_mgr:to_atom_assets(Type), Num}||[N, Type, Num] <-RewardList],
            do_daily_kill_reward(List, 1, RewardList1, Time);
        _ ->
            do_daily_kill_reward(List, 1, Time)
    end.

%% 按照后台配置处理
do_daily_kill_reward([], _, _, _) -> ok;
do_daily_kill_reward([#rank_role{role_id = RoleId} | L], N, RewardList, Time) ->
    Title = "击杀排行榜奖励",
    Msg = util:fbin("恭喜您获得今日击杀排行榜第~w,获得以下奖励请查收.", [N]),
    case catch lists:keyfind(N, 1, RewardList) of
        {_, Type, Num} ->
            Items = [#p_assets{type = Type, num = Num}],
            mail_mgr:send(0, RoleId, Title, Msg, Items, Time),
            do_daily_kill_reward(L, N + 1, RewardList, Time);
        _ ->
            ok
    end.
%% 按照基本配置处理
do_daily_kill_reward([], _, _) -> ok;
do_daily_kill_reward([#rank_role{role_id = RoleId} | L], N, Time) ->
    Title = "击杀排行榜奖励",
    Msg = util:fbin("恭喜您获得今日击杀排行榜第~w,获得以下奖励请查收.", [N]),
    case catch zoo_hit_rank_setting:get_data(N) of
        {List} when is_list(List) ->
            Items = [#p_assets{type = Type, num = Num} || {Type, Num} <- List],
            mail_mgr:send(0, RoleId, Title, Msg, Items, Time),
            do_daily_kill_reward(L, N + 1, Time);
        _ ->
            ok
    end.

%% 每周击杀榜发奖励
do_week_kill_reward() ->
    List = lookup_list(?rank_kill_week),
    Time = date:unixtime(),
    case setting_mgr:get(?setting_kill_rank_week) of
        {ok, RewardList} when RewardList =/= [] -> 
            RewardList1 = [{N, mail_mgr:to_atom_assets(Type), Num}||[N, Type, Num] <-RewardList],
            do_week_kill_reward(List, 1, RewardList1, Time);
        _ ->
            do_week_kill_reward(List, 1, Time)
    end.

do_week_kill_reward(Time) ->
    List = lookup_list(?rank_kill_week),
    case setting_mgr:get(?setting_kill_rank_week) of
        {ok, RewardList} when RewardList =/= [] -> 
            RewardList1 = [{N, mail_mgr:to_atom_assets(Type), Num}||[N, Type, Num] <-RewardList],
            do_week_kill_reward(List, 1, RewardList1, Time);
        _ ->
            do_week_kill_reward(List, 1, Time)
    end.

%% 按照后台配置处理
do_week_kill_reward([], _, _, _) -> ok;
do_week_kill_reward([#rank_role{role_id = RoleId} | L], N, RewardList, Time) ->
    Title = "击杀排行榜奖励",
    Msg = util:fbin("恭喜您获得本次击杀排行榜第~w,获得以下奖励请查收.", [N]),
    case catch lists:keyfind(N, 1, RewardList) of
        {_, Type, Num} ->
            Items = [#p_assets{type = Type, num = Num}],
            mail_mgr:send(0, RoleId, Title, Msg, Items, Time),
            do_week_kill_reward(L, N + 1, RewardList, Time);
        _ ->
            ok
    end.
%% 按照基本配置处理
do_week_kill_reward([], _, _) -> ok;
do_week_kill_reward([#rank_role{role_id = RoleId} | L], N, Time) ->
    Title = "击杀排行榜奖励",
    Msg = util:fbin("恭喜您获得本次击杀排行榜第~w,获得以下奖励请查收.", [N]),
    case catch zoo_hit_weekly_rank_setting:get_data(N) of
        {List} when is_list(List) ->
            Items = [#p_assets{type = Type, num = Num} || {Type, Num} <- List],
            mail_mgr:send(0, RoleId, Title, Msg, Items, Time),
            do_week_kill_reward(L, N + 1, Time);
        _ ->
            ok
    end.



%% 每日大奖赛榜发奖励
do_daily_great_reward() ->
    List = lookup_list(?rank_great_match),
    do_daily_great_reward(List, 1, date:unixtime()).

do_daily_great_reward(Time) ->
    List = lookup_list(?rank_great_match),
    do_daily_great_reward(List, 1, Time).

do_daily_great_reward([], _, _) -> ok;
do_daily_great_reward([#rank_role{role_id = RoleId} | L], N, Time) ->
    Title = "大奖赛每日排行榜奖励",
    Msg = util:fbin("恭喜您获得今日大奖赛排行榜第~w,获得以下奖励请查收.", [N]),
    case catch arena_rank_daily_reward_setting:get_data(N) of
        {List} when is_list(List) ->
            Items = [#p_assets{type = Type, num = Num} || {Type, Num} <- List],
            mail_mgr:send(0, RoleId, Title, Msg, Items, Time),
            do_daily_great_reward(L, N + 1, Time);
        _ ->
            ok
    end.


%% 每周大奖赛榜发奖励
do_week_great_reward() ->
    List = lookup_list(?rank_week_great_match),
    do_week_great_reward(List, 1, date:unixtime()).

do_week_great_reward(Time) ->
    List = lookup_list(?rank_week_great_match),
    do_week_great_reward(List, 1, Time).

do_week_great_reward(_, 2, _) -> ok;
do_week_great_reward([], _, _) -> ok;
do_week_great_reward([#rank_role{role_id = RoleId} | L], N, Time) ->
    Title = "大奖赛每周排行榜奖励",
    Msg = util:fbin("恭喜您获得本周大奖赛排行榜第~w,获得以下奖励请查收.", [N]),
    {List} = arena_rank_weekly_reward_setting:get_data(N),
    Items = [#p_assets{type = Type, num = Num} || {Type, Num} <- List],
    mail_mgr:send(0, RoleId, Title, Msg, Items, Time),
    do_week_great_reward(L, N + 1, Time).

