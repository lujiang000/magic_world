%%----------------------------------------------------
%% 全服账目统计
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(account_mgr).
-behaviour(gen_server).
-export([start_link/0
         ,charge/3
         ,output/2
         ,get_account/0
         ,change/0
         ,save/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state,  {
        coin_charge = 0           %%金币充值(分)
        ,gold_charge = 0          %%钻石充值
        ,first_charge = 0         %%1元礼包充值
        ,gift_charge = 0          %%贵族礼包充值
        ,charge_coin = 0          %% 充值产出的金币
        ,first_coin =  0          %% 1元礼包产出的金币
        ,share_coin = 0           %% 分享产出的金币
        ,invite_coin = 0          %% 邀请好友产出的金币
        ,alms_coin = 0            %% 救济金产出的金币
        ,vip_alms_coin = 0        %% vip 产出的救济金金币
        ,open_fire = 0            %% 解锁炮产出的金币
        ,gift_coin = 0            %% 贵族礼包产出的金币
        ,area_coin = 0            %% 竞技场产出的金币
        ,id_card_coin = 0         %% 身份证验证产出金币
        ,guide_task =  0          %% 新手任务产出金币
        ,guide_login_coin = 0     %% 新手礼包产出金币
        ,guide_login_tel = 0      %% 新手礼包产出话费
        ,login_coin = 0           %% 登陆转盘产出金币
        ,login_tel = 0            %% 登陆转盘产出话费
        ,vip_login_coin = 0       %% 登陆转盘产出金币
        ,vip_login_tel = 0        %% 登陆转盘产出话费
        ,gift_code_coin = 0       %% 礼包码产出金币
        ,gift_code_redbag = 0     %% 礼包码产出红包（毛）
        ,time = 0
    }
).



-include("common.hrl").

change() ->
    ?MODULE ! change.

save() ->
    ?MODULE ! save.

%% 充值统计
charge(Type, Rmb, Coin) ->
    ?MODULE ! {charge, Type, Rmb, Coin}.

%% 其他产出统计
output(Type, Coin) when is_integer(Coin)->
    ?MODULE ! {output, Type, Coin};
output(Type, List) ->
    case lists:keyfind(coin, 1, List) of
        {coin, Coin} ->
            ?MODULE ! {output, Type, Coin};
        _ ->
            ok
    end,
    case lists:keyfind(tel_fare, 1, List) of
        {tel_fare, Tel} ->
            ?MODULE ! {output, tel_fare, Type, Tel};
        _ ->
            ok
    end,
    case lists:keyfind(red_bag, 1, List) of
        {red_bag, RedBag} ->
            ?MODULE ! {output, red_bag, Type, RedBag};
        _ ->
            ok
    end.

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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
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


%% 充值相关统计
handle_info({charge, ?charge_coin, Rmb, Coin}, State = #state{coin_charge = Charge, charge_coin = Coin1}) ->
    {noreply, State#state{coin_charge = Charge + Rmb, charge_coin = Coin1 + Coin}};
handle_info({charge, ?charge_gold, Rmb, Coin}, State = #state{gold_charge = Charge, charge_coin = Coin1}) ->
    {noreply, State#state{gold_charge = Charge + Rmb, charge_coin = Coin1 + Coin}};
handle_info({charge, ?charge_first, Rmb, Coin}, State = #state{first_charge = Charge, first_coin = Coin1}) ->
    {noreply, State#state{first_charge = Charge + Rmb, first_coin = Coin1 + Coin}};
handle_info({charge, ?charge_gift, Rmb, Coin}, State = #state{gift_charge = Charge, gift_coin = Coin1}) ->
    {noreply, State#state{gift_charge = Charge + Rmb, gift_coin = Coin1 + Coin}};

%% 其他产出统计
handle_info({output, ?charge_gift, Coin}, State = #state{gift_coin = Coin1}) ->
    {noreply, State#state{gift_coin = Coin1 + Coin}};
handle_info({output, ?invite_coin, Coin}, State = #state{invite_coin = Coin1}) ->
    {noreply, State#state{invite_coin = Coin1 + Coin}};
handle_info({output, ?share_coin, Coin}, State = #state{share_coin = Coin1}) ->
    {noreply, State#state{share_coin = Coin1 + Coin}};
handle_info({output, ?alms_coin, Coin}, State = #state{alms_coin = Coin1}) ->
    {noreply, State#state{alms_coin = Coin1 + Coin}};
handle_info({output, ?vip_alms_coin, Coin}, State = #state{vip_alms_coin = Coin1}) ->
    {noreply, State#state{vip_alms_coin = Coin1 + Coin}};
handle_info({output, ?open_fire, Coin}, State = #state{open_fire = Coin1}) ->
    {noreply, State#state{open_fire = Coin1 + Coin}};
handle_info({output, ?area_coin, Coin}, State = #state{area_coin = Coin1}) ->
    {noreply, State#state{area_coin = Coin1 + Coin}};
handle_info({output, ?id_card_coin, Coin}, State = #state{id_card_coin = Coin1}) ->
    {noreply, State#state{id_card_coin = Coin1 + Coin}};
handle_info({output, ?guide_task_coin, Coin}, State = #state{guide_task = Coin1}) ->
    {noreply, State#state{guide_task = Coin1 + Coin}};
handle_info({output, ?guide_login_coin, Coin}, State = #state{guide_login_coin = Coin1}) ->
    {noreply, State#state{guide_login_coin = Coin1 + Coin}};
handle_info({output, ?login_coin, Coin}, State = #state{login_coin = Coin1}) ->
    {noreply, State#state{login_coin = Coin1 + Coin}};
handle_info({output, ?vip_login_coin, Coin}, State = #state{vip_login_coin = Coin1}) ->
    {noreply, State#state{vip_login_coin = Coin1 + Coin}};
handle_info({output, ?gift_code_coin, Coin}, State = #state{gift_code_coin = Coin1}) ->
    {noreply, State#state{gift_code_coin = Coin1 + Coin}};

%% 话费产出
handle_info({output, tel_fare, ?guide_login_coin, Tel}, State = #state{guide_login_tel = Tel1}) ->
    {noreply, State#state{guide_login_tel = Tel1 + Tel}};
handle_info({output, tel_fare, ?login_coin, Tel}, State = #state{login_tel = Tel1}) ->
    {noreply, State#state{login_tel = Tel1 + Tel}};
handle_info({output, tel_fare, ?vip_login_coin, Tel}, State = #state{vip_login_tel = Tel1}) ->
    {noreply, State#state{vip_login_tel = Tel1 + Tel}};

%% 红包产生
handle_info({output, red_bag, ?gift_code_coin, RedBag}, State = #state{gift_code_redbag = RedBag1}) ->
    {noreply, State#state{gift_code_redbag = RedBag + RedBag1}};


%% 处理金币改变比例
handle_info(change, State = #state{charge_coin = ChargeCoin, first_coin = FirstCoin, share_coin = ShareCoin, invite_coin = InviteCoin, alms_coin = AlmsCoin, vip_alms_coin = VipAlmsCoin, open_fire = OpenFire, gift_coin = GiftCoin, area_coin = AreaCoin, id_card_coin = IdCardCoin, guide_task = GuideTask}) ->
    {noreply, State#state{charge_coin = ChargeCoin * 10, first_coin = FirstCoin * 10, share_coin = ShareCoin * 10, invite_coin = InviteCoin * 10, alms_coin = AlmsCoin * 10, vip_alms_coin = VipAlmsCoin * 10, open_fire = OpenFire * 10, gift_coin = GiftCoin * 10, area_coin = AreaCoin * 10, id_card_coin = IdCardCoin * 10, guide_task = GuideTask * 10}};

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
    case db:get_row("select * from account_log where time = ?", [Time]) of
        {ok, [_, CoinCharge, FirstCharge, GiftCharge, GoldCharge, ChargeCoin, FirstCoin, ShareCoin, InviteCoin, AlmsCoin, VipAlmsCoin, OpenFire, GiftCoin, AreaCoin, IdCardCoin, GuideTask, GuideLoginCoin, GuideLoginTel, LoginCoin, LoginTel,  VipLoginCoin, VipLoginTel, GiftCodeCoin, GiftCodeRed, Time]} ->
            #state{coin_charge = CoinCharge, first_charge = FirstCharge, gift_charge = GiftCharge, gold_charge = GoldCharge, charge_coin = ChargeCoin, first_coin = FirstCoin, share_coin = ShareCoin, invite_coin = InviteCoin, alms_coin = AlmsCoin, vip_alms_coin = VipAlmsCoin, open_fire = OpenFire, gift_coin = GiftCoin, area_coin = AreaCoin, id_card_coin = IdCardCoin, guide_task = GuideTask, guide_login_coin = GuideLoginCoin, guide_login_tel = GuideLoginTel, login_coin = LoginCoin, login_tel = LoginTel, vip_login_coin = VipLoginCoin, vip_login_tel = VipLoginTel, gift_code_coin = GiftCodeCoin, gift_code_redbag = GiftCodeRed, time = Time};
        _ ->
            #state{time = Time}
    end.

%% 入库
save(#state{coin_charge = CoinCharge, first_charge = FirstCharge, gift_charge = GiftCharge, gold_charge = GoldCharge, charge_coin = ChargeCoin, first_coin = FirstCoin, share_coin = ShareCoin, invite_coin = InviteCoin, alms_coin = AlmsCoin, vip_alms_coin = VipAlmsCoin, open_fire = OpenFire, gift_coin = GiftCoin, area_coin = AreaCoin, id_card_coin = IdCardCoin, guide_task = GuideTask, guide_login_coin = GuideLoginCoin, guide_login_tel = GuideLoginTel, login_coin = LoginCoin, login_tel = LoginTel, vip_login_coin = VipLoginCoin, vip_login_tel = VipLoginTel, gift_code_coin = GiftCodeCoin, gift_code_redbag = GiftCodeRed, time = Time}) ->
    case db:get_row("select id from account_log where time = ?", [Time]) of
        {ok, [Id]} ->
            db:exec("replace into account_log(id, coin_charge, first_charge, gift_charge, gold_charge, charge_coin, first_coin, share_coin, invite_coin, alms_coin, vip_alms_coin, open_fire, gift_coin, area_coin, id_card_coin, guide_task, guide_login_coin, guide_login_tel, login_coin, login_tel, vip_login_coin, vip_login_tel, gift_code_coin, gift_code_redbag, time) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", [Id, CoinCharge, FirstCharge, GiftCharge, GoldCharge, ChargeCoin, FirstCoin, ShareCoin, InviteCoin, AlmsCoin, VipAlmsCoin, OpenFire, GiftCoin, AreaCoin, IdCardCoin, GuideTask, GuideLoginCoin, GuideLoginTel, LoginCoin, LoginTel,  VipLoginCoin, VipLoginTel, GiftCodeCoin, GiftCodeRed, Time]);
        _ ->
            db:exec("insert into account_log(coin_charge, first_charge, gift_charge, gold_charge, charge_coin, first_coin, share_coin, invite_coin, alms_coin, vip_alms_coin, open_fire, gift_coin, area_coin, id_card_coin, guide_task, guide_login_coin, guide_login_tel, login_coin, login_tel, vip_login_coin, vip_login_tel, gift_code_coin, gift_code_redbag, time) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", [CoinCharge, FirstCharge, GiftCharge, GoldCharge, ChargeCoin, FirstCoin, ShareCoin, InviteCoin, AlmsCoin, VipAlmsCoin, OpenFire, GiftCoin, AreaCoin, IdCardCoin, GuideTask, GuideLoginCoin, GuideLoginTel, LoginCoin, LoginTel,  VipLoginCoin, VipLoginTel, GiftCodeCoin, GiftCodeRed, Time])
    end.

