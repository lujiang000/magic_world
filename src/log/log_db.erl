%%----------------------------------------------------
%% 本服日志处理进程
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(log_db).
-behaviour(gen_server).
-export([
        start_link/1
        ,log/3
        ,test/0
        ,test1/0
        ,log/4
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        list = []  %% {Type, Pool, values}
    }).

-define(max_num, 1000).  %% 单次处理最大数据量
-define(time_tick, 5000). %% 没5秒处理一次数据

-include("common.hrl").

start_link(N) ->
    gen_server:start_link(?MODULE, [N], []).


log(Type, Pool, Data) ->
    Data1 = formart_insert_values(Data),
    case Type of
        role ->
            db_log_11 ! {log, {Type, Pool, Data1}};
        gold_cost_log ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        mail_log ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        jd_card ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        phone_card ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        _ ->
            db_log_12 ! {log, {Type, Pool, Data1}}

    end.

%% 人物金币日志压力比较大，用10个进程处理 前期只用5个进程处理
log(_RoleID, Type = coin_cost_log, Pool, Data) ->
    Data1 = formart_insert_values(Data),
    N = sys_rand:rand(1, 5),
    case N  of
        1 ->
            db_log_1 ! {log, {Type, Pool, Data1}};
        2 ->
            db_log_2 ! {log, {Type, Pool, Data1}};
        3 ->
            db_log_3 ! {log, {Type, Pool, Data1}};
        4 ->
            db_log_4 ! {log, {Type, Pool, Data1}};
        5 ->
            db_log_5 ! {log, {Type, Pool, Data1}};
        6 ->
            db_log_6 ! {log, {Type, Pool, Data1}};
        7 ->
            db_log_7 ! {log, {Type, Pool, Data1}};
        8 ->
            db_log_8 ! {log, {Type, Pool, Data1}};
        9 ->
            db_log_9 ! {log, {Type, Pool, Data1}};
        _ ->
            db_log_10 ! {log, {Type, Pool, Data1}}
    end.




init([N]) ->
    process_flag(trap_exit, true),
    case N =< 10 of
        true ->
            erlang:process_flag(min_bin_vheap_size, 1024*1024),
            erlang:process_flag(min_heap_size, 1024*1024),
            erlang:process_flag(priority, high);
        _ ->
            ok
    end,
    Name = list_to_atom(lists:concat(["db_log_", N])),
    register(Name, self()),
    put(self_name, Name),
    State = #state{},
    ?INFO("[~w] 已经启动", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State = #state{list = List}) ->
    NewList = do_db(?max_num, List, []),
    case List of
        [] -> ok;
        _ ->
            erlang:send_after(?time_tick, self(), tick)
    end,
    {noreply, State#state{list = NewList}};

handle_info({log, Log}, State = #state{list = List}) ->
    case List of
        [] -> 
            erlang:send_after(?time_tick, self(), tick);
        _ -> ok
    end,
    {noreply, State#state{list = [Log | List]}};

handle_info(collect, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{list = List}) ->
    do_db(-1, List, []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_db(_N, [], List) -> 
    insert_db(List),
    [];
do_db(0, L, List) -> 
    insert_db(List),
    L;
do_db(N, [{Type, Pool, Sql}| L], List) -> 
    NewList = case lists:keyfind(Type, 1, List) of
        {Type, Pool, L1} ->
            lists:keyreplace(Type, 1, List, {Type, Pool, [Sql | L1]});
        _ -> 
            [{Type, Pool, [Sql]} | List]
    end,
    do_db(N -1, L, NewList).


insert_db([]) -> ok;
insert_db([{Type, Pool, Values} | L]) ->
%%    ?ERROR_MSG("数据库入库类型:~w,长度:~w", [Type,length(Values)]),
    Sql = sql(Type),
    insert_db(Sql, Pool, Values),
    insert_db(L).

insert_db(Sql, _Pool, Values) ->
    List = string:join(Values, ","),
    Sql1 = util:flist(Sql, [List]),
%%    ?ERR("Sql:~ts", [list_to_binary(Sql1)]),
    case db:exec(list_to_binary(Sql1)) of
        ok -> ok;
        {error,{'EXIT',{mysql_timeout,5000,{}}}} -> ok;
        _Err ->
            ?ERR("mysql sql error:~ts:~w~n", [Sql1, _Err])
    end.


test() ->
    List = formart_insert_values([10001, 1, 2, 1, 1000]),
    [log_db:log(animal_log, insert, List)|| _<-lists:seq(1, 10000)].

test1() ->
    List = formart_insert_values([1, 1000, 100]),
    [log_db:log(room, insert, List)|| _<-lists:seq(1, 10000)].



%% 插入数据值转换
formart_insert_values(List) ->
    db:insert_vals_sql(List).
%%    List1 = lists:duplicate(length(List), "~ts"),
%%    List2 = string:join(List1, ","),
%%    List3 = lists:concat(["(", List2, ")"]),
%%    List4 = [emysql_util:encode(Value)|| Value<-List],
%%    util_1:flist(List3, List4).




sql(role) -> "replace into role (role_id, openid, icon, name, regist_time, gold, coin, type, parent_id, vip, login_time, off_time, charge, red_bag, lollipop, tel_fare, phone, phone_screat, off, exchange, channel, pay_openid, red_openid, max_coin, candy, lolly, info) values  ~ts";
sql(mail_log) -> "insert into mail_log(from_id, to_id, items, time) values ~ts";
sql(jd_card) -> "insert into jd_card(role_id, type, value, cami, price, time) values ~ts";
sql(phone_card) -> "insert into phone_card(role_id, type, value, phone, price, time) values ~ts";
sql(coin_cost_log) -> "insert into coin_cost_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(gold_cost_log) -> "insert into gold_cost_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(lollipop_log) -> "insert into lollipop_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(red_bag_log) -> "insert into red_bag_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(tel_fare_log) -> "insert into tel_fare_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(candy_log) -> "insert into candy_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(lolly_log) -> "insert into lolly_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(client_error_log) -> "insert into client_error_log (role_id, name, msg, time) values ~ts";
sql(coin_tree_log) -> "insert into coin_tree_log (role_id, cost_coin, add_coin, time) values ~ts";
sql(send_coin_log) -> "insert into send_coin_log (role_id, type, num, time) values ~ts";
sql(redbag_to_charge) -> "insert into redbag_to_charge (role_id, redbag, type, value, time) values ~ts";
sql(active_drop_log) -> "insert into active_drop_log (role_id, coin, rate, item, value, time) values ~ts";
sql(shp_exchange_log) -> "insert into shop_exchange_log (role_id, cost_type, cost_num, exchange_type, exchange_price, time) values ~ts";
sql(role_charge_exchange_log) -> "insert into role_charge_exchange_log(role_id, channel, charge, exchange, time) values ~ts";

sql(coin_back_log) -> "insert into coin_back_log (gold, role_id, time) values ~ts";
sql(cost_log) ->
    Table = log_mgr:lookup_table(cost_log),
    lists:concat(["insert into ", Table, " (cost, type, role_id, to_id, pay_time, bar_income, us_income) values ~ts"]);
sql(feedback_log) -> "insert into feedback_log (role_id, time, content) values ~ts".
