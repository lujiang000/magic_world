%%----------------------------------------------------
%% 人物每天充值和兑换统计
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_charge_exchange_account).
-behaviour(gen_server).
-export([start_link/0
        ,get_account/0
        ,charge/2
        ,exchange/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-record(role_charge_exchange, {
        role_id = 0      %% 人物id
        ,channel = 0     %% 渠道商id
        ,charge = 0      %% 充值（分）
        ,exchange = 0    %% 兑换（分）
        ,time = 0        %% 日期 0点
    }
).

-include("common.hrl").
-include("role.hrl").

%% 充值
charge(_Role = #role{role_id = RoleId, channel = Channel}, Num) ->
    case ets:lookup(role_charge_exchange, RoleId) of
        [Data = #role_charge_exchange{charge = Charge}] ->
            ets:insert(role_charge_exchange, Data#role_charge_exchange{charge = Charge + Num});
        _ ->
            ets:insert(role_charge_exchange, #role_charge_exchange{role_id = RoleId, channel = Channel, charge = Num, time = date:unixtime()})
    end.

%% 兑换
exchange(_Role = #role{role_id = RoleId, channel = Channel}, Num) ->
    case ets:lookup(role_charge_exchange, RoleId) of
        [Data = #role_charge_exchange{exchange = Exchange}] ->
            ets:insert(role_charge_exchange, Data#role_charge_exchange{exchange = Exchange + Num});
        _ ->
            ets:insert(role_charge_exchange, #role_charge_exchange{role_id = RoleId, channel = Channel, exchange = Num, time = date:unixtime()})
    end.


%% 后台调用
get_account() ->
    List = ets:tab2list(role_charge_exchange),
    [do_web_data(Data)|| Data<-List].


do_web_data(Data) ->
    List1 = record_info(fields, role_charge_exchange),
    [_ | List2] = erlang:tuple_to_list(Data),
    lists:zip(List1, List2).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(role_charge_exchange, [set, named_table, public, {keypos, #role_charge_exchange.role_id}]),
    dets:open_file(role_charge_exchange, [{file, "./dets/role_charge_exchange.dets"},  {keypos, #role_charge_exchange.role_id}, {type, set}]),
    ets:from_dets(role_charge_exchange, role_charge_exchange),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, #state{}}.

handle_call(get_account, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%% 0点入库
handle_info(next_day, State) ->
    List = ets:tab2list(role_charge_exchange),
    save(List),
    ets:delete_all_objects(role_charge_exchange),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    {noreply, State};


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    dets:delete_all_objects(role_charge_exchange),
    ets:to_dets(role_charge_exchange, role_charge_exchange),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% 入库
save([#role_charge_exchange{role_id = RoleId, channel = Channel, charge = Charge, exchange = Exchange, time = Time} | L]) -> 
    log_db:log(role_charge_exchange_log, insert, [RoleId, Channel, Charge, Exchange, Time]),
    save(L);
save([]) -> ok.




