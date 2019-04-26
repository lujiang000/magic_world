%%----------------------------------------------------
%% 人物活跃的和次留
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_account_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,registe/1
        ,login/1
        ,save/0
        ,get_account/0
        ,update/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-record(role_account_mgr, {
        channel = 0      %% 渠道商id
        ,registe = 0     %% 注册人数
        ,login = 0       %% 登陆人数
        ,login_list = [] %% 登陆id
        ,next_day = 0    %% 次日留存
        ,next_list = []  %% 登陆id
        ,seven_day = 0   %% 7日留存
        ,seven_list = [] %% 登陆id 
        ,time = 0        %% 日期 0点
        ,fifteen_day = 0   %% 15日留存
        ,fifteen_list = [] %% 登陆id 
    }
).

-include("common.hrl").
-include("role.hrl").

registe(Channel) ->
    ?MODULE ! {registe, Channel}.

save() ->
    ?MODULE ! save.

%% 后台调用
get_account() ->
    case catch gen_server:call(?MODULE, get_account) of
        {ok, Data} ->
            List1 = record_info(fields, state),
            [_ | List2] = erlang:tuple_to_list(Data),
            lists:zip(List1, List2);
        _ ->
            false
    end.


login(_Role = #role{role_id = Id, regist_time = Time1, channel = Channel}) ->
    Now = date:unixtime(),
    case date:day_diff(Time1, Now) of
        1 ->
            ?MODULE ! {login, next, Channel, Id};
        6 ->
            ?MODULE ! {login, seven, Channel, Id};
        14 ->
            ?MODULE ! {login, fifteen_day, Channel, Id};
        _ ->
            ok
    end,
    ?MODULE ! {login, today, Channel, Id}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(role_account_mgr, [set, named_table, public, {keypos, #role_account_mgr.channel}]),
    dets:open_file(role_account_mgr, [{file, "./dets/role_account_mgr.dets"},  {keypos, #role_account_mgr.channel}, {type, set}]),
    do_init(),
    dets:delete_all_objects(role_account_mgr),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    erlang:send_after(600 * 1000, self(), online_role),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, #state{}}.

handle_call(get_account, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.



%% 注册
handle_info({registe, Channel}, State) ->
    case catch ets:lookup(role_account_mgr, Channel) of
        [Account = #role_account_mgr{registe = Registe}] ->
            ets:insert(role_account_mgr, Account#role_account_mgr{registe = Registe + 1});
        [] ->
            ets:insert(role_account_mgr, #role_account_mgr{channel = Channel, registe = 1});
        _R ->
            ?ERR("注册统计查询错误：~w", [_R]),
            ok
    end,
    {noreply, State};
%% 登陆
handle_info({login, today, Channel, Id}, State) ->
    case catch ets:lookup(role_account_mgr, Channel) of
        [Account = #role_account_mgr{login = Login, login_list = List}] ->
            case lists:member(Id, List) of
                true -> 
                    ok;
                _ -> 
                    ets:insert(role_account_mgr, Account#role_account_mgr{login = Login + 1, login_list = [Id | List]})
            end;
        [] ->
            ets:insert(role_account_mgr, #role_account_mgr{channel = Channel, login = 1, login_list = [Id], time = date:unixtime(zero)});
        _R ->
            ?ERR("注册登陆查询错误：~w", [_R]),
            ok
    end,
    {noreply, State};
handle_info({login, next, Channel, Id}, State) ->
    case catch ets:lookup(role_account_mgr, Channel) of
        [Account = #role_account_mgr{next_day = Login, next_list = List}] ->
            case lists:member(Id, List) of
                true -> 
                    ok;
                _ -> 
                    ets:insert(role_account_mgr, Account#role_account_mgr{next_day = Login + 1, next_list = [Id | List]})
            end;
        [] ->
            ets:insert(role_account_mgr, #role_account_mgr{channel = Channel, next_day = 1, next_list = [Id], time = date:unixtime(zero)});
        _R ->
            ?ERR("注册登陆查询错误：~w", [_R]),
            ok
    end,
    {noreply, State};
handle_info({login, seven, Channel, Id}, State) ->
    case catch ets:lookup(role_account_mgr, Channel) of
        [Account = #role_account_mgr{seven_day = Login, seven_list = List}] ->
            case lists:member(Id, List) of
                true -> 
                    ok;
                _ -> 
                    ets:insert(role_account_mgr, Account#role_account_mgr{seven_day = Login + 1, seven_list = [Id | List]})
            end;
        [] ->
            ets:insert(role_account_mgr, #role_account_mgr{channel = Channel, seven_day = 1, seven_list = [Id], time = date:unixtime(zero)});
        _R ->
            ?ERR("注册登陆查询错误：~w", [_R]),
            ok
    end,
    {noreply, State};

handle_info({login, fifteen_day, Channel, Id}, State) ->
    case catch ets:lookup(role_account_mgr, Channel) of
        [Account = #role_account_mgr{fifteen_day = Login, fifteen_list = List}] ->
            case lists:member(Id, List) of
                true -> 
                    ok;
                _ -> 
                    ets:insert(role_account_mgr, Account#role_account_mgr{fifteen_day = Login + 1, fifteen_list = [Id | List]})
            end;
        [] ->
            ets:insert(role_account_mgr, #role_account_mgr{channel = Channel, fifteen_day = 1, fifteen_list = [Id], time = date:unixtime(zero)});
        _R ->
            ?ERR("注册登陆查询错误：~w", [_R]),
            ok
    end,
    {noreply, State};

handle_info(save, State) ->
    List = ets:tab2list(role_account_mgr),
    save(List),
    {noreply, State};

%% 0点入库
handle_info(next_day, State) ->
    List = ets:tab2list(role_account_mgr),
    save(List),
    ets:delete_all_objects(role_account_mgr),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    {noreply, State};

%% 在线人数统计
handle_info(online_role, State) ->
    Num = role_data:get_online_num(),
    db:exec("insert into role_online_log(num, time) values (?, ?)", [Num, date:unixtime()]),
    erlang:send_after(600 * 1000, self(), online_role),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    ets:to_dets(role_account_mgr, role_account_mgr),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% 入库
save([#role_account_mgr{channel = Channel, registe = Registe, login = Login, next_day = Next, seven_day = Seven, time = Time, fifteen_day = Fifteen} | L]) -> 
    %% 今日数据处理
    case db:get_row("select id from role_account_log where channel =  ? and time = ?", [Channel, Time]) of
        {ok, [Id]} ->
            db:exec("replace into role_account_log(id, channel, registe, login, time) values (?, ?, ?, ?, ?)", [Id, Channel, Registe, Login, Time]);
        _ ->
            db:exec("insert into role_account_log(channel, registe, login, time) values (?, ?, ?, ?)", [Channel, Registe, Login, Time])
    end,
    %% 次日数据处理
    db:exec("update role_account_log set next_day = ? where channel = ? and time = ?", [Next, Channel, Time - 86400]),
    %% 7日数据处理
    db:exec("update role_account_log set seven_day = ? where channel = ? and time = ?", [Seven, Channel, Time - 6 * 86400]),
    %% 15日
    db:exec("update role_account_log set fifteen_day = ? where channel = ? and time = ?", [Fifteen, Channel, Time - 14 * 86400]),
    save(L);
save([]) -> ok.


do_init() ->
    Fun =
    fun(Data) ->
            NewData = #role_account_mgr{} = update_var(Data),
            ets:insert(role_account_mgr, NewData),
            continue
    end,
    dets:traverse(role_account_mgr, Fun).


%% 升级版本
update_var({role_account_mgr, Channel, Regist, Login, LoginList, NextDay, NextList, SevenDay, SevenList, Time}) ->
    update_var({role_account_mgr, Channel, Regist, Login, LoginList, NextDay, NextList, SevenDay, SevenList, Time, 0, []});
update_var(Data = #role_account_mgr{}) -> Data.




update() ->
    case db:get_all("select channel, registe, time from role_account_log where time <= 1544112000") of
        {ok, List} ->
            update(List);
        _ ->
            ok
    end.

update([]) -> ok;
update([[Channel, Registe, Time] | L]) -> 
    Seven = case Registe > 1 of
        true ->  sys_rand:rand(1, Registe);
        _ -> 0
    end,
    case db:exec("update role_account_log set seven_day = ? where channel = ? and time = ?", [Seven, Channel, Time]) of
        ok -> 
            timer:sleep(300);
        _E ->
            ?ERR("Time:~w  :~w", [Time, _E])
    end,
    update(L).

