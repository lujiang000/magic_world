%%----------------------------------------------------
%% 排行处理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(rank_zone).
-behaviour(gen_server).
-export([start_link/1
        ,in_rank/3
        ,exit_rank/3
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {flag = 0}). %% 是否停止接收数据 0否，1是

-include("rank.hrl").
-include("all_pb.hrl").
-include("role.hrl").
-include("common.hrl").

start_link(N) ->
    gen_server:start_link(?MODULE, [N], []).

in_rank(Zone, Type, RankRole) -> 
    Zone ! {in_rank, Type, RankRole}.

exit_rank(Zone, Type, Id) ->
    Zone ! {exit_rank, Type, Id}.


init([N]) ->
    process_flag(trap_exit, true), 
    Name = list_to_atom(lists:concat(["rank_zone_", N])),
    register(Name, self()),
    State = #state{},
    {ok, State}.


handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 上榜处理
handle_info({in_rank, Type, NewData = #rank_role{id = Id, value1 = Value}}, State = #state{flag = 0}) ->
    Rank = #rank{list = RankList, len = Len, last_val = LastValue} = rank_mgr:lookup(Type),
    #rank_config{len = Max, keys = SortKeys} = rank:get_rank_config(Type),
    case lists:keyfind(Id, #rank_role.id, RankList) of
        false -> %% 当前角色不在榜上 试上榜
            case Value >= LastValue orelse Max > Len of
                false -> %% 条件不满足 无法上榜 直接返回
                    {noreply, State};
                true ->
                    ToSortList = [NewData | lists:reverse(RankList)],
                    {NewRankList, NewLastValue} = update_rank(Max, SortKeys, ToSortList),
                    NewRank = Rank#rank{list = NewRankList, last_val = NewLastValue},
                    update_ets(NewRank),
                    {noreply, State}
            end;
        _OldData -> %% 当前在榜上 数据有变化 更新榜上数据
            NRandList = lists:keyreplace(Id, #rank_role.id, RankList, NewData), %% 把榜上数据替换成最新数据
            ToSortList = lists:reverse(NRandList), %% 为确保数值一致情况下先上榜排行在前
            {NewRankList, NewLastValue} = update_rank(Max, SortKeys, ToSortList),
            NewRank = Rank#rank{list = NewRankList, last_val = NewLastValue},
            update_ets(NewRank),
            {noreply, State}
    end;

%% 下榜操作
handle_info({exit_rank, Type, Id}, State) ->
    Rank = #rank{list = RankList} = rank_mgr:lookup(Type),
    case lists:keyfind(Id, #rank_role.id, RankList) of
        false -> %% 没有在榜上
            {noreply, State};
        _ -> %% 在榜上 进行下榜操作
            NewRankList = lists:keydelete(Id, #rank_role.id, RankList),
            update_ets(Rank#rank{list = NewRankList, last_val = 0}),
            {noreply, State}
    end;
%% 停止接收数据
handle_info(stop_calc, State) ->
    {noreply, State#state{flag = 1}};
%% 开始接收数据
handle_info(start_calc, State) ->
    {noreply, State#state{flag = 0}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_rank(Max, SortKeys, ToSortList) ->
    SortList = util:keys_sort(SortKeys, ToSortList), %% 倒序让条件相同情况下 先上榜在前
    Len = length(SortList),
    if
        Max =:= 0 orelse Len < Max -> %% 排行榜未满，直接上榜
            {SortList, 0};
        Len =:= Max -> %% 排行榜刚刚满数
            [LastPlayer | _] = lists:reverse(SortList),
            FirstBasis = first_sort_key(SortKeys),
            {SortList, element(FirstBasis,LastPlayer)};
        Len =:= Max + 1 -> %% 排行榜超出指定长度，需去除最后一名
            [_N | AscendRankedData] = lists:reverse(SortList),
            [LastPlayer | _] = AscendRankedData,
            FirstBasis = first_sort_key(SortKeys),
            {lists:reverse(AscendRankedData), element(FirstBasis,LastPlayer)};
        true ->
            {lists:sublist(SortList, Max), 0}
    end.


update_ets(Rank = #rank{list = L}) ->
    Len = length(L),
    ets:insert(rank, Rank#rank{len = Len}).

%% 获取第一排行键
first_sort_key(SortKeys) ->
    case lists:reverse(SortKeys) of 
        [{_, Key} | _] -> Key;
        [Key | _] -> Key 
    end.

