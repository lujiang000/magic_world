%%----------------------------------------------------
%% 好友系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(friend_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,add_friend/1
        ,add_friend/2
        ,charge/2
        ,get_coin/1
        ,get_friend_info/4
        ,change/0
        ,old/0
        ,select/0
        ,animal_win/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("all_pb.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(friend_db_num, 10).  %% 好友数据库数量


-record(friend, {
        role_id = 0         %% 人物id 
        ,icon = ""          %% 头像
        ,name = ""          %% 名字
        ,parent_id = 0      %% 上级好友id
        ,list1 = []         %% 一级好友id列表
        ,list2 = []         %% 二级好友id列表
        ,list3 = []         %% 三级好友id列表
        ,list4 = []         %% 四级好友id列表
        ,num1 = 0           %% 一级好友数量
        ,num2 = 0           %% 二级好友数量
        ,num3 = 0           %% 三级好友数量
        ,num4 = 0           %% 四级好友数量
        ,coin = 0           %% 好友今天带来的福利 (金币)
        ,all_coin = 0       %% 好友总福利（金币)
        ,num = 0            %% 今日邀请好友的数量
        ,all_num = 0        %% 总的好友数量
    }
).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


change() ->
    ?MODULE ! change.

%% 增加邀请好友，做完任务才算
add_friend(0) ->
    ok;
add_friend(Id) ->
    ?MODULE ! {add_friend, Id}.


%% 增加好友,注册就算 Info:{RoleId, Name, Icon}
add_friend(_, 0) ->
    ok;
add_friend(Info, Id) ->
    ?MODULE ! {add_friend, Id, Info}.


%% 充值
charge(_, _) -> ok;
charge(Id, Rmb) -> 
    ?MODULE ! {charge, Id, Rmb}.

%% 获取可以获得的金币
get_coin(RoleId) ->
    case catch ets:lookup(friend, RoleId) of
        [#friend{coin = Coin}] -> Coin;
        _ -> 0
    end.

%% 击杀动物上级需要进行推送和加金币
animal_win(_Role = #role{parent_id = 0}, _Win) -> ok;
animal_win(_Role = #role{name = Name, icon = Icon, role_id = RoleId, parent_id = Id}, Win) when Win >= 20 -> 
    Add = erlang:trunc(Win * 5/100),
    case role_data:get_online_role(Id) of
        {ok, #online_role{socket_pid = Pid}} ->
            sys_conn:pack_send(Pid, 1153, #m_1153_toc{role_id = RoleId, name = Name, icon = Icon, num = Add});
        _ -> ok
    end,
    ?MODULE ! {add_coin, Id, Add};
animal_win(_, _) -> ok.

%% 获取第N级的好友列表信息
get_friend_info(RoleId, N, Page, PageNum) ->
    Setting = get_friend_setting(),
    Show = [Id||{Id, _, 1}<-Setting],
    case lookup(RoleId) of
        {ok, #friend{coin = Coin, list1 = List1, num1 = Num1, list2 = List2, num2 = Num2, list3 = List3, num3 = Num3, list4 = List4, num4 = Num4, all_num = _AllNum}} ->
            case lists:member(N, Show) of
                true ->
                    {List, Num} = case N of
                        1 -> {List1, Num1};
                        2 -> {List2, Num2};
                        3 -> {List3, Num3};
                        4 -> {List4, Num4}
                    end,
                    RoleList = do_look_up_friend(List, Num, Page, PageNum),
                    AllNum = case Show of
                        [1] -> Num1;
                        [1, 2] -> Num1 + Num2;
                        [1, 2, 3] -> Num1 + Num2 + Num3;
                        [1, 2, 3, 4] -> Num1 + Num2 + Num3 + Num4;
                        _ -> 0
                    end,
                    {Coin, AllNum, util:ceil(Num/PageNum), RoleList, Show};
                _ ->
                    {0, 0, 0, [], Show}
            end;
        _ ->
            {0, 0, 0, [], Show}
    end.

%% 获取好友配置
get_friend_setting() ->
    case setting_mgr:get(?setting_friend) of
        {ok, Setting} ->  [erlang:list_to_tuple(A)|| A <-Setting];
        _ -> [erlang:list_to_tuple(A)|| A <-?friend_setting]
    end.


%% 获取一页的好友信息列表
do_look_up_friend(List, N, Page, PageNum) ->
    Start = (Page - 1) * PageNum + 1,
    case Start > N of
        true -> [];
        _ ->
            List1 = lists:sublist(List, Start, PageNum),
            to_friend(List1, [])
            
    end.

%% 转换前端列表
to_friend([Id | L], List) ->
    case lookup(Id) of
        {ok, #friend{icon = Icon, name = Name}} ->
            to_friend(L, [#p_friend{role_id = Id, icon = Icon, name = Name} | List ]);
        _ ->
            to_friend(L, List)
    end;
to_friend([], List) -> List.



init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    do_new_ets(),
    do_new_dets(),
    dets_to_ets(),
    ?INFO("[~w] 启动完成", [?MODULE]),
%%    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), send_coin),
    erlang:send_after(date:next_diff(4, 0, 0) * 1000, self(), save_dets),
    {_, {_, M, S}} = erlang:localtime(),
    Time = (59 - M) * 60 + (60 - S),
    erlang:send_after(Time * 1000, self(), send_gift),
    State = #state{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.



%% 邀请成功一位好友 %% 做完任务
handle_info({add_friend, Id}, State) ->
    case lookup(Id) of
        {ok, Friend = #friend{num = Num}} ->
            save(Friend#friend{num = Num + 1});
        _ -> 
            ok
    end,
    {noreply, State};

%% 邀请成功一位好友 %% 注册
handle_info({add_friend, Id, Info}, State) ->
    add_friend1(Id, Info),
    {noreply, State};


%% 充值返利，没有上级的不给(分)
handle_info({charge, Id, Rmb}, State) ->
    Setting = get_friend_setting(),
    add_charge(Id, Rmb, 1, Setting),
    {noreply, State};

%% 每日4点保存一次数据
handle_info(save_dets, State) ->
    save_dets(),
    erlang:send_after(86400 * 1000, self(), save_dets),
    {noreply, State};



%% 每日0点发送返利
handle_info(send_coin, State) ->
    send_coin(),
    erlang:send_after(86400 * 1000, self(), send_coin),
    {noreply, State};

%% 整点发送礼包
handle_info(send_gift, State) ->
    send_gift(),
    erlang:send_after(3600 * 1000, self(), send_gift),
    {noreply, State};


%% 增加金币
handle_info({add_coin, Id, Add}, State) ->
    case lookup(Id) of
        {ok, Friend = #friend{coin = Coin, all_coin = AllCoin}} -> 
            save(Friend#friend{coin = Coin + Add, all_coin = AllCoin + Add});
        _ -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    ets_to_dets(),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 发送礼包
send_gift() ->
    send_gift(0).

send_gift(?friend_db_num) -> ok;
send_gift(N) ->
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    Ms = ets:fun2ms(fun(Friend = #friend{num = Num}) when Num > 0 -> 
                Friend
        end),
    List = ets:select(Name, Ms),
    Time = date:unixtime(),
    do_send_gift(List, Time),
    ets:to_dets(Name, Name),
    send_gift(N + 1).

do_send_gift([], _) -> ok;
do_send_gift([Friend = #friend{role_id = RoleID, num = Num} | L], Time) when Num > 0-> 
    send_gift_mail(RoleID, Num, Time),
    save(Friend#friend{num = 0}),
    do_send_gift(L, Time);
do_send_gift([_ | L], Time) ->
    do_send_gift(L, Time).

send_gift_mail(RoleID, Num, Time) -> 
    account_mgr:output(?invite_coin, 500 * Num),
    Items = [#p_assets{type = coin, num = 500 * Num}, #p_assets{type = gold, num = Num}, #p_assets{type = locking, num = Num}],
    mail_mgr:send(0, RoleID, "邀请好友奖励", util:fbin("您成功邀请了~w位好友，获得奖励：", [Num]), Items, Time).


%% 发送返利
send_coin() ->
    send_coin(0).

%% 对每张ets表进行发送
send_coin(?friend_db_num) -> ok;
send_coin(N) ->
    Time = date:unixtime(),
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    Ms = ets:fun2ms(fun(Friend = #friend{coin = Coin}) when Coin >= 1000 -> 
                Friend
        end),
    List = ets:select(Name, Ms),
    do_send_coin(List, Time),
    send_coin(N + 1).


do_send_coin([Friend = #friend{role_id = RoleID, coin = Coin} | L], Time) when Coin >= 1000-> 
    send_coin_mail(RoleID, Coin, Time),
    save(Friend#friend{coin = Coin rem 1000}),
    do_send_coin(L, Time);
do_send_coin([_ | L], Time) ->
    do_send_coin(L, Time);
do_send_coin([], _) -> ok.

%% 发送福利邮件
send_coin_mail(RoleID, Coin, Time) -> 
    Items = [#p_assets{type = red_bag, num = trunc(Coin/1000)}],
    mail_mgr:send(0, RoleID, "好友福利", "您的好友慷慨华贵，一掷千金，奖励：", Items, Time).


%% 增加好友 一级
add_friend1(0, _Id) -> ok;
add_friend1(ParentId, {Id, Name, Icon}) ->
    case lookup(Id) of   
        false ->
            save(#friend{role_id = Id, parent_id = ParentId, name = Name, icon = Icon}),
            case lookup(ParentId) of
                {ok, Friend = #friend{role_id = ParentId, parent_id = ParentId1, list1 = List, num1 = Num1, all_num = AllNum}} -> 
                    save(Friend#friend{role_id = ParentId, list1 = [Id | List], num1 = Num1 + 1, all_num = AllNum + 1}),
                    add_friend2(ParentId1, Id);
                _ ->
                    save(#friend{role_id = ParentId, list1 = [Id], num1 = 1, all_num = 1})
            end;
        _ ->
            ok
    end.

%% 增加好友 二级
add_friend2(0, _Id) -> ok;
add_friend2(ParentId, Id) ->
    case lookup(ParentId) of
        {ok, Friend = #friend{role_id = ParentId, parent_id = ParentId1, list2 = List, num2 = Num1, all_num = AllNum}} -> 
            save(Friend#friend{role_id = ParentId, list2 = [Id | List], num2 = Num1 + 1, all_num = AllNum + 1}),
            add_friend3(ParentId1, Id);
        _ ->
            ok
    end.

%% 增加好友 三级
add_friend3(0, _Id) -> ok;
add_friend3(ParentId, Id) ->
    case lookup(ParentId) of
        {ok, Friend = #friend{role_id = ParentId, parent_id = ParentId1, list3 = List, num3 = Num1, all_num = AllNum}} -> 
            save(Friend#friend{role_id = ParentId, list3 = [Id | List], num3 = Num1 + 1, all_num = AllNum + 1}),
            add_friend4(ParentId1, Id);
        _ ->
            ok
    end.
        
%% 增加好友 四级
add_friend4(0, _Id) -> ok;
add_friend4(ParentId, Id) ->
    case lookup(ParentId) of
        {ok, Friend = #friend{role_id = ParentId, list4 = List, num4 = Num1, all_num = AllNum}} -> 
            save(Friend#friend{role_id = ParentId, list4 = [Id | List], num4 = Num1 + 1, all_num = AllNum + 1});
        _ ->
            ok
    end.

%% 增加好友福利 处理四级
add_charge(0, _, _N, _) -> ok;
add_charge(_, _, 5, _) -> ok;
add_charge(Id, Rmb, N, Setting) ->
    case lists:keyfind(N, 1, Setting) of
        {N, Per, 1} when Per > 0->
            Add = trunc(Rmb * 100 * Per /100),
            case catch lookup(Id) of
                {ok, Friend = #friend{parent_id = ParentId, coin = Coin, all_coin = AllCoin}} -> 
                    NewFriend = Friend#friend{coin = Coin + Add, all_coin = AllCoin + Add},
                    save(NewFriend),
                    add_charge(ParentId, Rmb, N + 1, Setting);
                _ -> 
                    ok
            end;
        _ ->
            ok
    end.



%%根据id查看好友信息
lookup(RoleId) ->
    N = RoleId rem ?friend_db_num,
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    case ets:lookup(Name, RoleId) of
        [Data] -> {ok, Data};
        [] -> false;
        _R -> _R
    end.

%% 保存好友信息 
%% 注意： 必须要在进程内执行，避免脏数据
save(Data = #friend{role_id = RoleId}) ->
    N = RoleId rem ?friend_db_num,
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    ets:insert(Name, Data).

%% 初始化ets表
do_new_ets() ->
    do_new_ets(0).

do_new_ets(?friend_db_num) -> ok;
do_new_ets(N) ->
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    ets:new(Name, [set, named_table, public, {keypos, #friend.role_id}]),
    do_new_ets(N + 1).

%% 初始化dets表
do_new_dets() ->
    do_new_dets(0).

do_new_dets(?friend_db_num) -> ok;
do_new_dets(N) ->
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    dets:open_file(Name, [{file, lists:concat(["./dets/", Name, ".dets"])}, {keypos, #friend.role_id}, {type, set}]),
    do_new_dets(N + 1).

%% ets 到dets处理 并且关闭dets
ets_to_dets() ->
    ets_to_dets(0).

ets_to_dets(?friend_db_num) -> ok;
ets_to_dets(N) ->
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    ets:to_dets(Name, Name),
    util:close_dets(Name),
    ets_to_dets(N + 1).

%% dets 到ets处理
dets_to_ets() ->
    dets_to_ets(0).

dets_to_ets(?friend_db_num) -> ok;
dets_to_ets(N) ->
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    ets:from_dets(Name, Name),
    dets_to_ets(N + 1).

%% 保存数据
save_dets() ->
    save_dets(0).

save_dets(?friend_db_num) -> ok;
save_dets(N) ->
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    ets:to_dets(Name, Name),
    save_dets(N + 1).

%% 从mysql导入数据
old() ->
    case db:get_all("select role_id, name, icon, parent_id from role where parent_id <> 0 ") of
        {ok, List} ->
            old(List);
        _ ->
            ok
    end.

old([[RoleId, Name, Icon, ParentId] | L]) ->
    add_friend({RoleId, Name, Icon}, ParentId),
    old(L);
old([]) -> ok.


select() ->
    select(0, []).

select(?friend_db_num, List) -> List;
select(N, List1) ->
    Name = erlang:list_to_atom(lists:concat([friend_, N])),
    Ms = ets:fun2ms(fun(Friend = #friend{num2 = Num2, num3 = Num3, num4 = Num4}) when Num2 > 0 orelse Num3 > 0 orelse Num4 > 0-> 
                Friend
        end),
    List = ets:select(Name, Ms),
    select(N + 1, List ++ List1).


