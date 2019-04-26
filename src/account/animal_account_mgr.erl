%%----------------------------------------------------
%% 动物园数据统计管理系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(animal_account_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,update_animal_pw/2
        ,get_account/0
        ,update_task/1
        ,update_bonus/2
        ,change/0
        ,save/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        p = 0             %% 打动物总投入金币
        ,w = 0            %% 打动物总产出金币
        ,lollipop = 0     %% 打动物棒棒糖总产出个数
        ,red_bag = 0      %% 打动物红包总产出（分）
        ,task_coin = 0      %% 触发任务金币奖励
        ,task_tel = 0       %% 触发任务话费券奖励
        ,bonus_coin = 0        %% 彩金金币产出
        ,bonus_lollipop = 0    %% 彩金棒棒糖产出
        ,bonus_tel = 0         %% 彩金话费券产出
        ,candy = 0          %% 打动物小棒棒糖总产出个数
        ,lolly = 0          %% 打动物中棒棒糖总产出个数
        ,time = 0
    }
).

-include("common.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

change() ->
    ?MODULE ! change.

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


update_animal_pw(P, W) when is_integer(W)->
    ?MODULE ! {update_animal_pw, P, W, 0, 0, 0, 0};
update_animal_pw(P, List) ->
    W1 = case lists:keyfind(coin, 1, List) of
        {coin, Coin} -> Coin;
        _ -> 0
    end,
    W2 = case lists:keyfind(red_bag, 1, List) of
        {red_bag, Bag} -> Bag;
        _ -> 0
    end,
    W3 = case lists:keyfind(lollipop, 1, List) of
        {lollipop, Num} -> Num;
        _ -> 0
    end,
    W4 = case lists:keyfind(candy, 1, List) of
        {candy, Candy} -> Candy; 
        _ -> 0
    end,
    W5 = case lists:keyfind(lolly, 1, List) of
        {lolly, Lolly} -> Lolly;
        _ -> 0
    end,
    ?MODULE ! {update_animal_pw, P, W1, W3, W2, W4, W5}.


%% 跟新任务产出
update_task(List) ->
    case lists:keyfind(coin, 1, List) of
        {coin, Coin} ->
            ?MODULE ! {update_task,  coin, Coin};
        _ ->
            case lists:keyfind(tel_fare, 1, List) of
                {tel_fare, Num} ->
                    ?MODULE ! {update_task,  tel_fare, Num};
                _ ->
                    ok
            end
    end.


%% 更新彩金产出
update_bonus(Type, Num) when Type =:= coin orelse Type =:= lollipop orelse Type =:= tel_fare->
    ?MODULE ! {update_bonus, Type, Num};
update_bonus(_, _) -> ok.


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    erlang:process_flag(min_bin_vheap_size, 1024*1024),
    erlang:process_flag(min_heap_size, 1024*1024),
    erlang:process_flag(priority, high),
    State = do_init(date:unixtime(zero)),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(get_account, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({update_animal_pw, P, W, L, R, Candy, Lolly}, State = #state{p = P1, w = W1, lollipop = L1, red_bag =  R1, candy = C, lolly = Lolly0}) ->
    {noreply, State#state{p = P1 + P, w = W1 + W, lollipop = L1 + L, red_bag = R1 + R, candy = C + Candy, lolly = Lolly0 + Lolly}};

handle_info({update_task, coin, Num}, State = #state{task_coin = Coin}) ->
    {noreply, State#state{task_coin = Coin + Num}};
handle_info({update_task, tel_fare, Num}, State = #state{task_tel = Tel}) ->
    {noreply, State#state{task_tel = Tel + Num}};

handle_info({update_bonus, coin, Num}, State = #state{bonus_coin = Coin}) ->
    {noreply, State#state{bonus_coin = Coin + Num}};
handle_info({update_bonus, lollipop, Num}, State = #state{bonus_lollipop = L}) ->
    {noreply, State#state{bonus_lollipop = L + Num}};
handle_info({update_bonus, tel_fare, Num}, State = #state{bonus_tel = Tel}) ->
    {noreply, State#state{bonus_tel = Tel + Num}};


%% 处理金币改变比例
handle_info(change, State = #state{p = P, w = W, task_coin = TaskCoin, bonus_coin = BonusCoin}) ->
    {noreply, State#state{p = P * 10, w = W * 10, task_coin = TaskCoin * 10, bonus_coin = BonusCoin * 10}};

%% 0点入库
handle_info(next_day, State) ->
    save(State),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    {noreply, #state{time = date:unixtime(zero)}};

handle_info(save, State) ->
    save(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    save(State),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 初始化
do_init(Time) ->
    case db:get_row("select * from animal_account_log where time = ?", [Time]) of
        {ok, [_, P, W, Lollipop, Lolly, Candy, RedBag, TaskCoin, TaskTel, BonusCoin, BonusLollipop, BonusTel, Time]} ->
            #state{p = P, w = W, lollipop = Lollipop, lolly = Lolly, candy = Candy, red_bag = RedBag, task_coin = TaskCoin, task_tel = TaskTel, bonus_coin = BonusCoin, bonus_lollipop = BonusLollipop, bonus_tel = BonusTel, time = Time};
        _ ->
            #state{time = Time}
    end.

%% 入库
save(#state{p = P, w = W, lollipop = Lollipop, lolly = Lolly, candy = Candy, red_bag = RedBag, task_coin = TaskCoin, task_tel = TaskTel, bonus_coin = BonusCoin, bonus_lollipop = BonusLollipop, bonus_tel = BonusTel, time = Time}) -> 
    case db:get_row("select id from animal_account_log where time = ?", [Time]) of
        {ok, [Id]} ->
            db:exec("replace into animal_account_log(id, p, w, lollipop, lolly, candy, red_bag, task_coin, task_tel, bonus_coin, bonus_lollipop, bonus_tel, time) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", [Id, P, W, Lollipop, Lolly, Candy, RedBag, TaskCoin, TaskTel, BonusCoin, BonusLollipop, BonusTel, Time]);
        _ ->
            db:exec("insert into animal_account_log(p, w, lollipop, lolly, candy, red_bag, task_coin, task_tel, bonus_coin, bonus_lollipop, bonus_tel, time) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", [P, W, Lollipop, Lolly, Candy, RedBag, TaskCoin, TaskTel, BonusCoin, BonusLollipop, BonusTel, Time])
    end.



