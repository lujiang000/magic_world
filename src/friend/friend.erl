%%----------------------------------------------------
%% 好友系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(friend).
-behaviour(gen_server).
-export([start_link/0
        ,add_friend/2
        ,charge/2
        ,get_coin/1
        ,get_friend_info/2
        ,change/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-record(friend, {
        role_id = 0
        ,list = []
        ,coin = 0
        ,all_coin = 0
        ,num = 0
        ,all_num = 0
    }
).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


change() ->
    ?MODULE ! change.

%% 增加好友
add_friend(_RoleId, 0) ->
    ok;
add_friend(RoleId, Id) ->
    ?MODULE ! {add_friend, Id, RoleId}.

%% 充值
charge(0, _) -> ok;
charge(Id, Rmb) -> 
    ?MODULE ! {charge, Id, Rmb}.

%% 获取可以获得的金币
get_coin(RoleId) ->
    case catch ets:lookup(friend, RoleId) of
        [#friend{coin = Coin}] -> Coin;
        _ -> 0
    end.

%% 获取好友列表  需要优化
get_friend_info(RoleId, Page) ->
    PageNum = 8,
    Num1 = case db:get_one("select count(*) from role where parent_id = ?", [RoleId]) of
        {ok, Num} -> Num;
        _ -> 0
    end,
    Start = Page * PageNum,
    End = Page * PageNum + PageNum,
    All = util:ceil(Num1/PageNum),
    List1 = case db:get_all("select role_id, icon, name from role where parent_id = ? limit ?, ?", [RoleId, Start, End]) of
        {ok, List} -> to_friend(List);
        _ -> []
    end,
    {Num1, All, List1}.

to_friend(List) ->
    [#p_friend{role_id = Id, icon = to_string(Icon), name = Name}||[Id, Icon, Name] <-List].

to_string(undefined)  -> "";
to_string(_Icon)  -> _Icon.




init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(friend, [set, named_table, public, {keypos, #friend.role_id}]),
    dets:open_file(friend, [{file, "./dets/friend.dets"}, {keypos, #friend.role_id}, {type, set}]),
    ets:from_dets(friend, friend),
    ?INFO("[~w] 启动完成", [?MODULE]),
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), send_coin),
    {_, {_, M, S}} = erlang:localtime(),
    Time = (59 - M) * 60 + (60 - S),
    erlang:send_after(Time * 1000, self(), send_gift),
    State = #state{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 邀请成功一位好友
handle_info({add_friend, Id, RoleId}, State) ->
    case catch ets:lookup(friend, Id) of
        [] -> 
            Friend = #friend{role_id = Id, list = [RoleId], num = 1, all_num = 1},
            ets:insert(friend, Friend);
        [Friend = #friend{list = List, num = Num, all_num = AllNum}] -> 
            NewFriend = case lists:member(RoleId, List) of
                true ->
                    Friend;
                _ ->
                    Friend#friend{list = [RoleId | List], num = Num + 1, all_num = AllNum + 1}
            end,
            ets:insert(friend, NewFriend);
        _ -> ok
    end,
    {noreply, State};

%% 充值返利，没有上级的不给(分)
handle_info({charge, Id, Rmb}, State) ->
    Add = trunc(Rmb * 100 * 5 /100),
    case catch ets:lookup(friend, Id) of
        [Friend = #friend{coin = Coin, all_coin = AllCoin}] -> 
            NewFriend = Friend#friend{coin = Coin + Add, all_coin = AllCoin + Add},
            ets:insert(friend, NewFriend);
        _ -> 
            Friend = #friend{role_id = Id, coin = Add, all_coin = Add},
            ets:insert(friend, Friend)
    end,
    {noreply, State};


%% 每日0点发送返利
handle_info(send_coin, State) ->
    List = ets:tab2list(friend),
    Time = date:unixtime(),
    do_send_coin(List, Time),
    erlang:send_after(86400 * 1000, self(), send_coin),
    {noreply, State};

%% 整点发送礼包
handle_info(send_gift, State) ->
    List = ets:tab2list(friend),
    Time = date:unixtime(),
    do_send_gift(List, Time),
    erlang:send_after(3600 * 1000, self(), send_gift),
    ets:to_dets(friend, friend),
    {noreply, State};

%% 金币比例改变
handle_info(change, State) ->
    List = ets:tab2list(friend),
    [ets:insert(friend, Friend#friend{coin = Coin * 10, all_coin = AllCoin * 10})||Friend = #friend{coin = Coin, all_coin = AllCoin} <-List],
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    ets:to_dets(friend, friend),
    util:close_dets(friend),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 发送礼包
do_send_gift([], _) -> ok;
do_send_gift([Friend = #friend{role_id = RoleID, num = Num} | L], Time) when Num > 0-> 
    send_gift(RoleID, Num, Time),
    ets:insert(friend, Friend#friend{num = 0}),
    do_send_gift(L, Time);
do_send_gift([_ | L], Time) ->
    do_send_gift(L, Time).

send_gift(RoleID, Num, Time) -> 
    account_mgr:output(?invite_coin, 500 * Num),
    Items = [#p_assets{type = coin, num = 500 * Num}, #p_assets{type = gold, num = Num}, #p_assets{type = locking, num = Num}],
    mail_mgr:send(0, RoleID, "邀请好友奖励", util:fbin("您成功邀请了~w位好友，获得奖励：", [Num]), Items, Time).


%% 发送返利
do_send_coin([], _) -> ok;
do_send_coin([Friend = #friend{role_id = RoleID, coin = Coin} | L], Time) when Coin >= 1000-> 
    send_coin(RoleID, Coin, Time),
    ets:insert(friend, Friend#friend{coin = 0}),
    do_send_coin(L, Time);
do_send_coin([_ | L], Time) ->
    do_send_coin(L, Time).

send_coin(RoleID, Coin, Time) -> 
    Items = [#p_assets{type = red_bag, num = trunc(Coin/1000)}],
    mail_mgr:send(0, RoleID, "好友福利", "您的好友慷慨华贵，一掷千金，奖励：", Items, Time).

