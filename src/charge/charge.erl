%%----------------------------------------------------
%% @doc  充值处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(charge).
-export([do_charge/5
        ,charge_callback/2
        ,add_luck/1
        ,web_charge/3
        ,web_send_coin/3
        ,apply_send_coin/3
        ,redbag_to_charge/3
    ]).

-include("role.hrl").
-include("common.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").



-define(charge_status_start, 0).
-define(charge_status_finish, 1).

%% 支付类型
-define(charge_wxpubpay, 0).    %% 微信官方公众号支付
-define(charge_wft_wx, 1).      %% 威富通公众号支付
-define(charge_yao_zfb, 2).      %% 摇钱树支付宝支付扫码
-define(charge_yao_wx, 3).      %% 摇钱树微信扫码支付
-define(charge_yao_zfb_h5, 4).      %% 摇钱树支付宝h5
-define(charge_wft_wx_sm, 5).      %% 威富通微信扫码
-define(charge_yb_wx_sm, 6).      %% 易宝微信扫码
-define(charge_yao_yl, 7).      %% 摇钱树银联扫码
-define(charge_paysapi_wx, 8).      %% 个人微信转账支付 
-define(charge_paysapi_zfb, 9).      %% 个人支付宝转账支付 
-define(charge_yb_yl_sm_lx, 10).      %% 易宝银联扫码  轮询
-define(charge_wft_zfb, 11).      %% 威富通支付宝扫码


%% 微信官方支付接口
wxpubpay(OpenID, OrderID, Amount, RoleID, ChargeType) ->
  Url = "https://api.mch.weixin.qq.com/pay/unifiedorder",
  AppID = sys_env:get_env(payAppId),
  MchID = sys_env:get_env(payMachId),
  Key = sys_env:get_env(payMachKey),
  IP = sys_env:get_env(ip),
  NotifyUrl = "http://"++IP++":10000/wft",
  Body = case ChargeType of
           _ ->
             binary_to_list(unicode:characters_to_binary("充值"))
         end,
  Attach = json:encode([{role_id, RoleID}, {charge_type, ChargeType}]),
  SS = [
    {appid, AppID},
    {attach, Attach},
    {body, Body},
    {mch_id, MchID},
    {nonce_str, OrderID},
    {notify_url, NotifyUrl},
    {openid, OpenID},
    {out_trade_no, OrderID},
    {spbill_create_ip, IP},
    {total_fee, Amount},
    {trade_type, "JSAPI"},
    {key, Key}
  ],
  RawSS = util:format_get_params(SS),
  Sign = string:to_upper(binary_to_list(util:md5(RawSS))),
  [_ | T] = lists:reverse(SS),
  Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
  Time = date:unixtime(),
  Headers = [{"content-type", "application/json;charset=utf-8"}],
  ContentType = "application/x-www-form-urlencoded",
  case httpc:request(post, {Url, Headers, ContentType, Xml}, [{timeout, 5000}], []) of
    {ok, {_, _, Result}} ->
      case util:get_xml_info(Result, "return_code") of
        {ok, "SUCCESS"} ->
            case util:get_xml_info(Result, "prepay_id") of
                {ok, PrepayID} ->
                    Return = [
                        {appId, unicode:characters_to_binary(AppID)},
                        {nonceStr, unicode:characters_to_binary(OrderID)},
                        {package, unicode:characters_to_binary("prepay_id="++PrepayID)},
                        {signType, unicode:characters_to_binary("MD5")},
                        {timeStamp, unicode:characters_to_binary(integer_to_list(Time))},
                        {key, Key}],
                    Sign2 = string:to_upper(binary_to_list(util:md5(util:format_get_params(Return)))),
                    [_|T2] = lists:reverse(Return),
                    Json = json:encode(lists:reverse([{'paySign', unicode:characters_to_binary(Sign2)} | T2])),
                    {ok, Json};
                _ ->
                    {false, ?error_busy}
            end;
        _ ->
            {ok, Msg} = util:get_xml_info(Result, "return_msg"),
            ?ERR("====:~ts~n",[Msg]),
            {false, ?error_busy}
      end;
    _R ->
      ?ERR("_r:~p",[_R]),
      {false, ?error_busy}
  end.

%% 威富通公众号支付 %% OrderNo订单号 Amount 单位分 Attach::binary()是附加信息json体 例如 {"role_id":20001, "charge_type":red_bag}
wft_gzh(ChannelType, OpenID, OrderNo, Amount, RoleID, ChargeType) ->
    Url = "https://pay.swiftpass.cn/pay/gateway",
    AppID = sys_env:get_env(payAppId),
    MchID = sys_env:get_env(payMachId),
    Key = sys_env:get_env(payMachKey),
    IP = sys_env:get_env(ip),
    Body = case ChargeType of
        _ ->
            binary_to_list(unicode:characters_to_binary("充值"))
    end,
    Service = case ChannelType of
        gzh -> "pay.weixin.jspay";
        sm -> "pay.weixin.native";
        zfb -> "pay.alipay.native"
    end,
    Amt = integer_to_list(Amount),
    NonceStr = string:to_upper(erlang:binary_to_list(util:md5(OrderNo))),
    NotifyUrl = lists:concat(["http://", IP, ":", 10000, "/wft"]),
    Attach = json:encode([{role_id, RoleID}, {charge_type, ChargeType}]),
    SS = case ChannelType of
        gzh ->
            [
                {attach, Attach},
                {body, Body},
                {is_raw, 1},
                {mch_create_ip, IP},
                {mch_id, MchID},
                {nonce_str, NonceStr},
                {notify_url, NotifyUrl},
                {out_trade_no, OrderNo},
                {service, Service},
                {sub_appid, AppID},
                {sub_openid, OpenID},
                {total_fee, Amt},
                {key, Key}
            ];
        _ ->
            [
                {attach, Attach},
                {body, Body},
                {mch_create_ip, IP},
                {mch_id, MchID},
                {nonce_str, NonceStr},
                {notify_url, NotifyUrl},
                {out_trade_no, OrderNo},
                {service, Service},
                {sign_type, "MD5"},
                {total_fee, Amt},
                {key, Key}
            ]
    end,
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    case catch httpc:request(post, {Url, Headers, ContentType, Xml}, [{timeout, 5000}], []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "result_code") of
                {ok, "0"} ->
                    StringInfo =  case ChannelType of
                        gzh -> "pay_info";
                        sm -> "code_url";
                        zfb -> "code_url"
                    end,
                    case util:get_xml_info(Result, StringInfo) of
                        {ok, PayInfo} ->
                            {ok, PayInfo};
                        {error, ErrorID} ->
                            {false, ErrorID};
                        _Err ->
                            {false, 1}
                    end;
                _->
                    case catch util:get_xml_info(Result, "message") of
                        {ok, Message} ->
                            case OpenID of
                                "" ->
                                    ok;
                                _ ->
                                    ?ERR("威富通接口返回错误描述:~w: ~ts: ~ts", [ChannelType, Message, Xml])
                            end;
                        _ ->
                            case catch util:get_xml_info(Result, "err_msg") of
                                {ok, Message} ->
                                    case OpenID of
                                        "" ->
                                            ok;
                                        _ ->
                                            ?ERR("威富通接口返回错误描述:~w: ~ts: ~ts", [ChannelType, Message, Xml])
                                    end;
                                _ ->
                                    ok
                            end
                    end,
                    {false, 1}
            end;
        _Err ->
            {false, 1}
    end.



%% 摇钱树支付宝微信支付
yao_zfb(ChannelType, ChargeType, Amount, OrderID, SelfIp) ->
    Url = "https://opay.arsomon.com:28443/vipay/reqctl.do",
    MchID = sys_env:get_env(yqsMachId),
    Key = sys_env:get_env(yqsMachKey),
    IP = sys_env:get_env(ip),
    Body = case ChargeType of
        _ ->
            binary_to_list(unicode:characters_to_binary("充值"))
    end,
    Service =  case ChannelType of
        zfb -> "ali.activescan.pay";
        wx -> "wx.js.pay";
        zfb_h5 -> "ali.h5.pay";
        yl -> "union.activescan.pay"
    end,
    Amt = float_to_list(Amount/100, [{decimals, 2}]),
    NotifyUrl = lists:concat(["http://", IP, ":", 10000, "/yao_zfb"]),
    SS = 
    case ChannelType of
        zfb_h5 ->
            [
                {amount, Amt},
                {goods, Body},
                {ip, SelfIp},
                {mch_id, MchID},
                {notify_url, NotifyUrl},
                {order_no, OrderID},
                {service, Service},
                {key, Key}
            ];
        _ ->
            [
                {amount, Amt},
                {goods, Body},
                {mch_id, MchID},
                {notify_url, NotifyUrl},
                {order_no, OrderID},
                {service, Service},
                {key, Key}
            ]
    end,
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    case catch httpc:request(post, {Url, Headers, ContentType, Xml}, [{timeout, 5000}], []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "res_code") of
                {ok, "100"} ->
                    UrlString =  case ChannelType of
                        wx -> "url";
                        zfb_h5 -> "pay_url";
                        _ -> "code_url"
                    end,
                    case util:get_xml_info(Result, UrlString) of
                        {ok, CodeUrl} ->
                            {ok, CodeUrl};
                        {error, ErrorID} ->
                            {false, ErrorID};
                        _Err ->
                            {false, 1}
                    end;
                _->
                    case catch util:get_xml_info(Result, "res_msg") of
                        {ok, Message} ->
                            ?ERR("摇钱树接口返回错误描述:~w:~ts", [ChannelType, Message]),
                            {false, 1};
                        _ ->
                            {false, 1}
                    end
            end;
        _R ->
            ?ERR("摇钱树接口返回错误描述:~w: ~w", [ChannelType, _R]),
            {false, 1}
    end.

%% 个人转账支付
paysapi(ReturnUrl, OrderID, OrderUid, Money, Type) ->
    IP = sys_env:get_env(ip),
    Uid = sys_env:get_env(paysapi_uid),
    Token = sys_env:get_env(paysapi_token),
    GoodsName = "buy_bean",
    IsType = case Type of
            ?charge_paysapi_wx ->
                2;
            ?charge_paysapi_zfb ->
                1
        end,
    NotifyUrl = lists:concat(["http://", IP, ":", 10000, "/paysapi"]),
    Price = erlang:float_to_list(Money/100,[{decimals, 2}]),
    String = lists:concat([GoodsName, IsType, NotifyUrl, OrderID, OrderUid, Price, ReturnUrl, Token, Uid]),
    Sign = erlang:binary_to_list(util:md5(String)),
    List = [
        {uid, erlang:list_to_binary(Uid)}
        ,{price, erlang:list_to_binary(Price)}
        ,{istype, IsType}
        ,{notify_url, erlang:list_to_binary(NotifyUrl)}
        ,{return_url, erlang:list_to_binary(ReturnUrl)}
        ,{orderid, erlang:list_to_binary(OrderID)}
        ,{orderuid, OrderUid}
        ,{goodsname, erlang:list_to_binary(GoodsName)}
        ,{key, erlang:list_to_binary(Sign)}
    ],
    Json = json:encode([{url, <<"https://pay.bbbapi.com/">>} | List]),
    {ok, erlang:binary_to_list(Json)}.


%% 易宝微信扫码
yb_pay(Type, _OpenID, OrderID, Amount, RoleID, ChargeType) ->
    {Url, Type1, Other} = case Type of
      ?charge_yb_wx_sm ->
          {"https://www.u6u8.com/yskapi/Payment/WxChartPAPayment", "WXPAY", []};
      ?charge_yb_yl_sm_lx ->
          {"https://www.u6u8.com/yskapi/Payment/placeanorder2", "UNIONPAY", [{'L1_amoutType', "2"}]}
  end,
  MchID = sys_env:get_env(ybMachId),
%%  MchID = "4001104637",
  IP = sys_env:get_env(ip),
  NotifyUrl = "http://"++IP++":10000/yb",
  Key = sys_env:get_env(ybMachKey),
  Body = case ChargeType of
           _ ->
               binary_to_list(unicode:characters_to_binary(lists:concat(["订单编号:", OrderID])))
         end,
  Amt = float_to_list(Amount/100, [{decimals, 2}]),
  Attach = erlang:binary_to_list(json:encode([{role_id, RoleID}, {charge_type, ChargeType}])),
  SS = [
    {'P1_bizType', "AppPay"},
    {'P2_orderId', OrderID},
    {'P3_customerNumber', MchID},
    {'P4_payType', "SCAN"},
    {'P5_orderAmount', Amt},
    {'P6_currency', "CNY"},
    {'P7_authcode', "1"},
    {'P8_appType',  Type1},
    {'P9_notifyUrl', NotifyUrl},
    {'P10_successToUr', ""},
    {'P11_orderIp', IP},
    {'P12_goodsName', Body},
    {'P13_goodsDetail', ""},
    {'P14_desc', Attach},
    {key, Key}
  ],
  SS1 = "&" ++ string:join([util:to_list(H) || {_, H} <- SS], "&"),
  Sign = binary_to_list(util:md5(SS1)),
  [_ | T] = lists:reverse(SS),
  Xml = lists:reverse([{sign, Sign}|T]) ++ Other,
  Params = string:join([atom_to_list(Atom) ++ "=" ++ util:to_list(Val) || {Atom, Val} <- Xml], "&"),
%%  Time = date:unixtime(),
  Headers = [{"content-type", "application/json;charset=utf-8"}],
  ContentType = "application/x-www-form-urlencoded",
  case httpc:request(post, {Url, Headers, ContentType, Params}, [{timeout, 5000}], []) of
    {ok, {_, _, Result}} ->
        case json:decode(erlang:list_to_binary(Result), [{object_format, proplist}]) of
            List when is_list(List) ->
                case lists:keyfind(<<"rt2_retCode">>, 1, List) of
                    {_, <<"0000">>} ->
                        case lists:keyfind(<<"rt8_qrcode">>, 1, List) of
                            {_, CodeUrl} ->
                                {ok, binary_to_list(CodeUrl)};
                            _ ->
                                {false, ?error_busy}
                        end;
                    _ ->
                        case lists:keyfind(<<"rt3_retMsg">>, 1, List) of
                            {_, Msg} ->
                                ?ERR("====:~ts~n",[Msg]),
                                {false, ?error_busy};
                            _ ->
                                {false, ?error_busy}
                        end
                end;
            _ ->
                {false, ?error_busy}
        end;
    _ ->
        {false, ?error_busy}
 end.






%% 玩家充值
do_charge(_Role = #role{role_id = RoleId, pay_openid = OpenID, ip = Ip}, Type, Num, ChargeType, ReturnUrl) ->
    Now = date:unixtime(),
    N = sys_rand:rand(1000, 9999),
    Id = lists:concat([RoleId, N, Now]),
    case db:exec("insert into charge_log (id, role_id, charge_rmb, charge_type, type, time, status) values(?, ?, ?, ?, ?, ?, ?)", [Id, RoleId, Num, ChargeType, Type, date:unixtime(), ?charge_status_start]) of
        ok ->
            case ChargeType of
                ?charge_wxpubpay ->
                    wxpubpay(OpenID, Id, Num, RoleId, Type);
                ?charge_wft_wx ->
                    wft_gzh(gzh, OpenID, Id, Num, RoleId, Type);
                ?charge_wft_wx_sm ->
                    wft_gzh(sm, OpenID, Id, Num, RoleId, Type);
                ?charge_wft_zfb ->
                    wft_gzh(zfb, OpenID, Id, Num, RoleId, Type);
                ?charge_yb_wx_sm ->
                    yb_pay(ChargeType, OpenID, Id, Num, RoleId, Type);
                ?charge_yb_yl_sm_lx ->
                    yb_pay(ChargeType, OpenID, Id, Num, RoleId, Type);
                ?charge_yao_zfb ->
                    yao_zfb(zfb, Type, Num, Id, util:to_ip_string(Ip));
                ?charge_yao_wx ->
                    yao_zfb(wx, Type, Num, Id, util:to_ip_string(Ip));
                ?charge_yao_zfb_h5 ->
                    yao_zfb(zfb_h5, Type, Num, Id, util:to_ip_string(Ip));
                ?charge_yao_yl ->
                    yao_zfb(yl, Type, Num, Id, util:to_ip_string(Ip));
                ?charge_paysapi_wx ->
                    paysapi(ReturnUrl, Id, RoleId, Num, ChargeType);
                ?charge_paysapi_zfb ->
                    paysapi(ReturnUrl, Id, RoleId, Num, ChargeType);
                99 ->
                    {ok, "SUCCESS"}
            end;
        _ ->
            {false, ?error_busy}
    end.

web_charge(RoleId, Type, Num) ->
    Now = date:unixtime(),
    N = sys_rand:rand(1000, 9999),
    Id = lists:concat([RoleId, N, Now]),
    Num1 = Num * 100,
    case db:exec("insert into charge_log (id, role_id, charge_rmb, charge_type, type, time, status) values(?, ?, ?, ?, ?, ?, ?)", [Id, RoleId, Num1, 99, Type, date:unixtime(), ?charge_status_start]) of
        ok ->
            web_callback:do_pay_charge(Id, RoleId, Type, Num1);
        _ ->
            false
    end.



%% 充值回调
%% 金币
charge_callback(Role = #role{vip_charge = VipCharge, role_id = RoleID, vip = Vip, charge = Charge, parent_id = ParentId, first_charge = FirstCharge, luck = Luck, luck_num = LuckNum, channel = Channel, charge_reward_tomorrow = Reward}, {Id, Type = ?charge_coin, Num}) ->
    case db:get_one("select status from charge_log where id = ?", [Id]) of
        {ok, ?charge_status_start} ->
            #vip_welfare{charge_gold = Per} = vip:get_vip_welfare(Vip),
            Value1 = trunc(Num * 10000 /100),
            Value2 = trunc(Value1 * Per /100),
            Value3 = case FirstCharge of
                0 -> trunc(Value1/10);
                _ -> 0
            end,
            Value = Value1 + Value3,
            {ok, NewRole} = role_lib:do_add_coin(Role, Value),
            NewVip = vip:get_lev(VipCharge + Num),
            case NewVip =:= Vip of
                true -> ok;
                _ ->
                    sys_conn:pack_send(1124, #m_1124_toc{vip = NewVip})
            end,
            Now = date:unixtime(),
            db:exec("update charge_log set status = ? , call_time = ?,value = ?, send = ? where id = ?", [?charge_status_finish, Now, Value1, Value2 + Value3, Id]),
            case Channel of
                0 -> ok;
                _ ->
                    db:exec("insert into channel_charge_log(channel_id, role_id, charge_rmb, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleID, Num, Type, Now])
            end,
            sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = coin, num = Value}]}),
            friend_mgr:charge(ParentId, Num),
            account_mgr:charge(Type, Num, Value + Value2),
            AllLuck = case setting_mgr:get(1) of
                {ok, 1} -> Luck;
                _ ->
                    trunc(Num/100) + Luck
            end,
            {NewLuck, NewLuckNum} = do_luck_num(AllLuck, LuckNum),
            NewRole1 = NewRole#role{vip_charge = VipCharge + Num, charge = Charge + Num, vip = NewVip, first_charge = 1, luck = NewLuck, luck_num = NewLuckNum, charge_reward_tomorrow = Reward + Value2},
            NewRole2 = role_lib:do_set_sheep(NewRole1),
            {ok, NewRole2};
        _ ->
            {ok, Role}
    end;



%% 钻石
charge_callback(Role = #role{vip_charge = VipCharge, role_id = RoleID, channel = Channel, charge = Charge, vip = Vip, parent_id = ParentId, first_charge = FirstCharge}, {Id, Type = ?charge_gold, Num}) ->
    case db:get_one("select status from charge_log where id = ?", [Id]) of
        {ok, ?charge_status_start} ->
            Value1 = trunc(Num * 10 /100),
            Value2 = case FirstCharge of
                0 -> trunc(Value1/10);
                _ -> 0
            end,
            Value = Value1 + Value2,
            {ok, NewRole} = role_lib:do_add_gold(Role, Value),
            NewVip = vip:get_lev(VipCharge + Num),
            case NewVip =:= Vip of
                true -> ok;
                _ ->
                    sys_conn:pack_send(1124, #m_1124_toc{vip = NewVip})
            end,
            Now = date:unixtime(),
            db:exec("update charge_log set status = ? ,call_time = ?,value = ?, send = ? where id = ?", [?charge_status_finish, Now, Value, Value2, Id]),
            case Channel of
                0 -> ok;
                _ ->
                    db:exec("insert into channel_charge_log(channel_id, role_id, charge_rmb, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleID, Num, Type, Now])
            end,
            sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = gold, num = Value}]}),
            friend_mgr:charge(ParentId, Num),
            account_mgr:charge(Type, Num, 0),
            NewRole1 = NewRole#role{vip_charge = VipCharge + Num, charge = Charge + Num, vip = NewVip, first_charge = 1},
            NewRole2 = role_lib:do_set_sheep(NewRole1),
            {ok, NewRole2};
        _ ->
            {ok, Role}
    end;

%% 首冲礼包
charge_callback(Role = #role{charge = Charge, parent_id = ParentId, role_id = RoleID, channel = Channel, first_gift = First}, {Id, Type = ?charge_first, Num}) ->
    case db:get_one("select status from charge_log where id = ?", [Id]) of
        {ok, ?charge_status_start} ->
            case First of
                0 -> 
                    List = [{gold, 150}, {coin, 20000}],
                    {ok, NewRole} = role_lib:do_add(Role, List),
                    Now = date:unixtime(),
                    db:exec("update charge_log set status = ? , call_time = ? where id = ?", [?charge_status_finish, Now, Id]),
                    case Channel of
                        0 -> ok;
                        _ ->
                            db:exec("insert into channel_charge_log(channel_id, role_id, charge_rmb, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleID, Num, Type, Now])
                    end,
                    sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = Item, num = Value}||{Item, Value} <-List]}),
                    friend_mgr:charge(ParentId, Num),
                    account_mgr:charge(Type, Num, 20000),
                    {ok, NewRole#role{charge = Charge + Num, first_gift = 1}};
                _ ->
                    Now = date:unixtime(),
                    db:exec("update charge_log set status = ? , call_time = ? where id = ?", [?charge_status_finish, Now, Id]),
                    role_black:add_black(RoleID),
                    {ok, Role}
            end;
        _ ->
            {ok, Role}
    end;

%% 活动礼包
charge_callback(Role = #role{charge = Charge, parent_id = ParentId, role_id = RoleID, channel = Channel}, {Id, Type = ?charge_active, Num}) ->
    case db:get_one("select status from charge_log where id = ?", [Id]) of
        {ok, ?charge_status_start} ->
            case role_lib:get_value(Role, ?daily_girl_active) of
                0 ->
                    List = [{gold, 100}, {coin, 380000}, {self_horn, 2}, {gold_pick, 2}],
                    {ok, NewRole} = role_lib:do_add(Role, List),
                    Now = date:unixtime(),
                    db:exec("update charge_log set status = ? , call_time = ? where id = ?", [?charge_status_finish, Now, Id]),
                    case Channel of
                        0 -> ok;
                        _ ->
                            db:exec("insert into channel_charge_log(channel_id, role_id, charge_rmb, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleID, Num, Type, Now])
                    end,
                    sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = Item, num = Value}||{Item, Value} <-List]}),
                    friend_mgr:charge(ParentId, Num),
                    account_mgr:charge(Type, Num, 0),
                    NewRole1 = role_lib:add_value(NewRole#role{charge = Charge + Num}, ?daily_girl_active),
                    {ok, role_lib:do_set_sheep(NewRole1)};
                _ ->
                    charge_callback(Role, {Id, ?charge_coin, Num})
            end;
        _ ->
            {ok, Role}
    end;


%% 贵族礼包
charge_callback(Role = #role{charge = Charge, gift = Gift = #role_gift{end_time = EndTime}, parent_id = ParentId, role_id = RoleID, channel = Channel}, {Id, Type = ?charge_gift, Num}) ->
    case db:get_one("select status from charge_log where id = ?", [Id]) of
        {ok, ?charge_status_start} ->
            Now = date:unixtime(),
            NewGift = case Now >= EndTime of
                true ->
                    Zero = date:unixtime(zero),
                    Gift#role_gift{buy_time = Now, end_time = Zero + 30 * 86400};
                _ ->
                    Gift#role_gift{end_time = EndTime + 30 * 86400}
            end,
            Now = date:unixtime(),
            db:exec("update charge_log set status = ? , call_time = ? where id = ?", [?charge_status_finish, Now, Id]),
            case Channel of
                0 -> ok;
                _ ->
                    db:exec("insert into channel_charge_log(channel_id, role_id, charge_rmb, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleID, Num, Type, Now])
            end,
            sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = []}),
            friend_mgr:charge(ParentId, Num),
            account_mgr:charge(Type, Num, 0),
            NewRole = role_lib:do_set_sheep(Role#role{charge = Charge + Num, gift = NewGift}),
            {ok, NewRole};
        _ ->
            {ok, Role}
    end.

%% 红包抵扣
redbag_to_charge(Role, Type, Num) ->
    RedBag = trunc(Num/10),
    Flow = functions_mgr:get_exchange_flow(), %% 元
    case Flow * 100 >= Num of
        true ->
            role_lib:send_buff_begin(),
            case role_lib:do_cost(Role, [{red_bag, RedBag}]) of
                {ok, NewRole} ->
                    functions_mgr:delete_exchange_flow(trunc(Num/100)),
                    {ok, Add, NewRole1} = do_redbag_charge(NewRole, Type, Num),
                    role_lib:send_buff_flush(),
                    {ok, Add, NewRole1};
                {false, Reason} ->
                    role_lib:send_buff_clean(),
                    {false, Reason}
            end;
        _ ->
            {false, ?error_busy}
    end.

do_redbag_charge(Role = #role{vip_charge = VipCharge, role_id = RoleID, vip = Vip, first_charge = FirstCharge, luck = Luck, luck_num = LuckNum, charge_reward_tomorrow = Reward}, Type = ?charge_coin, Num) ->
    #vip_welfare{charge_gold = Per} = vip:get_vip_welfare(Vip),
    Value1 = trunc(Num * 10000 /100),
    Value2 = trunc(Value1 * Per /100),
    Value3 = case FirstCharge of
        0 -> trunc(Value1/10);
        _ -> 0
    end,
    Value4 = trunc(Value1 * 0.01),
    Value = Value1 + Value3 + Value4,
    {ok, NewRole} = role_lib:do_add_coin(Role, Value),
    NewVip = vip:get_lev(VipCharge + Num),
    case NewVip =:= Vip of
        true -> ok;
        _ ->
            sys_conn:pack_send(1124, #m_1124_toc{vip = NewVip})
    end,
    sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = coin, num = Value - Value4}]}),
    AllLuck = case setting_mgr:get(1) of
        {ok, 1} -> Luck;
        _ ->
            trunc(Num/100) + Luck
    end,
    {NewLuck, NewLuckNum} = do_luck_num(AllLuck, LuckNum),
    log_db:log(redbag_to_charge, insert, [RoleID, Num, Type, Value, date:unixtime()]),
    NewRole1 = NewRole#role{vip_charge = VipCharge + Num, vip = NewVip, first_charge = 1, luck = NewLuck, luck_num = NewLuckNum, charge_reward_tomorrow = Reward + Value2},
    {ok, Value4, NewRole1};

do_redbag_charge(Role = #role{vip_charge = VipCharge, role_id = RoleID, vip = Vip, first_charge = FirstCharge}, Type = ?charge_gold, Num) ->
    Value1 = trunc(Num * 10 /100),
    Value2 = case FirstCharge of
        0 -> trunc(Value1/10);
        _ -> 0
    end,
    Value = Value1 + Value2,
    {ok, NewRole} = role_lib:do_add_gold(Role, Value),
    NewVip = vip:get_lev(VipCharge + Num),
    case NewVip =:= Vip of
        true -> ok;
        _ ->
            sys_conn:pack_send(1124, #m_1124_toc{vip = NewVip})
    end,
    log_db:log(redbag_to_charge, insert, [RoleID, Num, Type, Value, date:unixtime()]),
    sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = gold, num = Value}]}),
    NewRole1 = NewRole#role{vip_charge = VipCharge + Num, vip = NewVip, first_charge = 1},
    {ok, 0, NewRole1}.




%% 是否触发幸运
do_luck_num(AllLuck, LuckNum) when AllLuck < 200 ->
    {AllLuck, LuckNum};
do_luck_num(AllLuck, LuckNum) ->
    case sys_rand:rand(1, 100) =< 5 of
        true ->
            do_luck_num(AllLuck - 200, LuckNum + 1);
        _ ->
            do_luck_num(AllLuck - 200, LuckNum)
    end.

add_luck(Id) when is_integer(Id) ->
    case role_data:get_online_role(Id) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, add_luck, []});
        _ ->
            ok
    end;
add_luck(Role) ->
    {ok, Role#role{luck_num = 1}}.


%% 赠送，扣除指定资产
web_send_coin(RoleID, Type, Num) ->
    case role_data:get_online_role(RoleID) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, apply_send_coin, [Type, Num]}),
            true;
        _ ->
            case role_data:get_role_from_dets(RoleID) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_send_coin(Role1, Type, Num),
                    role:do_change({charge, apply_send_coin}, NewRole, Role1),
                    role_data:save_to_db(NewRole),
                    true;
                _ ->
                   false 
            end
    end.

%% 增加资产
apply_send_coin(Role = #role{role_id = RoleId}, Type, Num) when Num > 0 ->
    {ok, NewRole} = case Type of
        1 -> 
            sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = coin, num = Num}]}),
            role_lib:do_add_coin(Role, Num);
        2 ->
            sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = gold, num = Num}]}),
            role_lib:do_add_gold(Role, Num)
    end,
    log_db:log(send_coin_log, insert, [RoleId, Type, Num, date:unixtime()]),
    {ok, NewRole};
%% 扣除资产
apply_send_coin(Role = #role{role_id = RoleId, coin = Coin, gold = Gold}, Type, Num) ->
    NewRole = case Type of
        1 -> Role#role{coin = max(0, Coin + Num)};
        2 -> Role#role{gold = max(0, Gold + Num)}
    end,
    log_db:log(send_coin_log, insert, [RoleId, Type, Num, date:unixtime()]),
    {ok, NewRole}.
    





