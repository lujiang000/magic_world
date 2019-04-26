%%%-------------------------------------------------------------------
%%% @author nwb
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 七月 2016 下午6:34
%%%-------------------------------------------------------------------
-module(lib_juhe).
-author("nwb").

-include_lib("xmerl/include/xmerl.hrl").

-define(URL, "http://op.juhe.cn/ofpay/mobile/onlineorder").

-include("common.hrl").

-export([
    direct_recharge/3
    ,check_phone/2
    ,recharge_phone_state/1
    ,check_flow/2
    ,flow_recharge/3
    ,get_flow_products/0
    ,io_flows/0
    ,jd_card/2
    ,get_jdk_info/1
    ,send_msg/2
    ,test/0
    ,robot_reply/0
    ,robot/1
    ,stock/0
    ,get_qb_list/0
    ,get_qb/3
    ,get_qb_state/1
]).

-define(juhekey, "ae02449fa59b713e8927ed270998cbfd").  %%聚合礼品卡key
-define(juheopenid, "JH59ef701f72c4e1c3cbc991266f36862a"). %%聚合openid
-define(juheacc, "13428281"). %% 聚合账号
-define(juhetpl, "93167"). %% 聚合信息模板
-define(juhesjkey, "e6e821af0efadc521ba4fbf07d51533a"). %% 聚合话费直冲key
-define(juhemsgkey, "a8443292c5d8f5cd7ea14f49ac940c4e"). %% 聚合短信的key


test() ->
    Url = "http://v.juhe.cn/flow/recharge",
    Key = "b566829d6be717ff3f844fa8036d2a29",
    OpenID = logic_mgr:get_juheopenid(),
    Mobile = "15626066789",
    Pid = "34",
    OrderID = time:now(micro),
    Md5Sorce = lists:concat([OpenID,Key,Mobile,Pid,OrderID]),
    Md5 = u_md5:encode(Md5Sorce),

    PropList = [
        {phone, Mobile},
        {pid, Pid},
        {orderid, OrderID},
        {key, Key},
        {sign, Md5}],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            check_result(Result);
        _ ->
            {error, 1}
    end.

%% 检查手机号和充值金额是否合法
check_phone(Mobile, CardNum) ->
    Url = "http://op.juhe.cn/ofpay/mobile/telcheck",
    Key = sys_env:get_env(juhesjkey),
    PropList = [
        {phoneno, Mobile},
        {cardnum, CardNum},
        {key, Key}],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            check_result(Result);
        _ ->
            {error, 1}
    end.

%% 已经检查成功进行充值
direct_recharge(Mobile, CardNum, OrderID) ->
    Url = ?URL,
    Key = sys_env:get_env(juhesjkey),
    OpenID = sys_env:get_env(juheopenid),
    Md5Sorce = lists:concat([OpenID,Key,Mobile,CardNum,OrderID]),
    Md5 = binary_to_list(util:md5(Md5Sorce)),
    PropList = [
        {phoneno, Mobile},
        {cardnum, CardNum},
        {orderid, OrderID},
        {key, Key},
        {sign, Md5}],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            check_result(Result);
        _ ->
            {error, 1}
    end.

check_flow(Mobile, Flow1) ->
    case Flow1 of
        R when R =:= 300 orelse R =:= 360 ->
            Flow = 500,
            Url = "http://v.juhe.cn/flow/telcheck",
            Key = logic_mgr:get_juhellkey(),
            PropList = [
                {phone, Mobile},
                {key, Key}],
            Params = sdk:format_http_get_params(PropList),
            case sdk:http(post, Url, Params) of
                {ok, Result} ->
                    get_flow_product_id(Result, Flow);
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 39}
    end.

flow_recharge(Mobile, ProductID, OrderID) ->
    Url = "http://v.juhe.cn/flow/recharge",
    Key = logic_mgr:get_juhellkey(),
    OpenID = logic_mgr:get_juheopenid(),
    Md5Sorce = lists:concat([OpenID,Key,Mobile,ProductID,OrderID]),
    Md5 = u_md5:encode(Md5Sorce),

    PropList = [
        {phone, Mobile},
        {pid, ProductID},
        {orderid, OrderID},
        {key, Key},
        {sign, Md5}],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            check_result(Result);
        _ ->
            {error, 1}
    end.

get_flow_product_id(Result, Flow) ->
    case rfc4627:decode(Result) of
        {ok, {obj, Json}, _} ->
            case lists:keyfind("error_code", 1, Json) of
                {_, ErrorCode} ->
                    case ErrorCode of
                        0 ->
                            [_, {_, [{obj, [_,_,_,_,_,{_,Flows}]}]}, _] = Json,
                            RRR =
                                lists:foldl(fun(X, Acc) ->
                                    {_, Item} = X,
                                    {_, Val} = lists:keyfind("v", 1, Item),
                                    case erlang:binary_to_integer(Val) of
                                        Flow ->
                                            {_, ID} = lists:keyfind("id", 1, Item),
                                            [erlang:binary_to_list(ID) | Acc];
                                        _ ->
                                            Acc
                                    end
                                            end, [], Flows),
                            case RRR of
                                [] ->
                                    false;
                                [Spec | _] ->
                                    {true,Spec}
                            end;
                        _ ->
                            false
                    end
            end;
        _ ->
            false
    end.


check_result(Result) ->
    case rfc4627:decode(Result) of
        {ok, {obj, Json}, _} ->
            case lists:keyfind("error_code", 1, Json) of
                {_, ErrorCode} ->
                    case ErrorCode of
                        0 ->
                            true;
                        208501 ->
                            %% 不允许充值的手机号码及金额
                            {error, 55};
                        208502 ->
                            %% 请求手机号和面值查询商品信息失败，请重试
                            {error, 56};
                        208503 ->
                            %% 运营商地区维护，暂不能充值
                            {error, 57};
                        208505 ->
                            %% 错误的手机号码
                            {error, 30};
                        208506 ->
                            %% 错误的充值金额
                            {error, 58};
                        208508 ->
                            %% 请求充值失败，请重试
                            {error, 59};
                        208509 ->
                            %% 错误的订单号
                            {error, 60};
                        208517 ->
                            %% 余额不足
                            ?ERR("聚合tmb余额不足", []),
                            {error, 46};
                        10014 ->
                            %% 聚合系统内部异常(调用充值类业务时，请务必联系客服或通过订单查询接口检测订单，避免造成损失)
                            ?ERR("聚合系统内部异常(调用充值类业务时，请务必联系客服或通过订单查询接口检测订单，避免造成损失) error_code:~p", [10014]),
                            {error, 61};
                        10020 ->
                            %% 接口维护
                            {error, 62};
                        10021 ->
                            %% 接口停用
                            {error, 63};
                        10011 ->
                            %% 当前IP请求超过限制
                            {error, 64};
                        10009 ->
                            %% 被禁止的KEY
                            {error, 65};
                        _ ->
                            case lists:keyfind("reason", 1, Json) of
                                {ok, Reason} ->
                                    ?ERR("聚合数据手机直冲出错 errorcode:~p, Reason:~ts~n", [ErrorCode, Reason]);
                                _ ->
                                    ?ERR("聚合数据手机直冲出错 errorcode:~p~n", [ErrorCode])
                            end,
                            false
                    end
            end;
        _ ->
            {error, 54}
    end.

%% 检测手机话费直冲状态
recharge_phone_state(OrderID) ->
    Url = "http://op.juhe.cn/ofpay/mobile/ordersta",
    Key = logic_mgr:get_juhesjkey(),
    PropList = [{orderid, OrderID},{key, Key}],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(get, Url, Params) of
        {ok, Result} ->
            case rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    case lists:keyfind("error_code", 1, Json) of
                        {_, 0} ->
                            case lists:keyfind("result", 1, Json) of
                                {_, R} ->
                                    case R of
                                        {obj, R1} ->
                                            %% R1 = [{"uordercash",<<"19.940">>},{"sporder_id",<<"J17071408551347660367696">>},{"game_state",<<"0">>}]
                                            case lists:keyfind("game_state", 1, R1) of
                                                {_, State} when is_binary(State)->
                                                    %% State = 1成功/9失败/0充值中
                                                    case catch erlang:binary_to_integer(State) of
                                                        NewState when is_integer(NewState) ->
                                                            {ok, NewState};
                                                        _ ->
                                                            ?ERR("check juhe state:~p", [State]),
                                                            {ok, State}
                                                    end;
                                                _Any->
                                                    ?ERR("check juhe state is not binary :~p", [_Any]),
                                                    {error, 1}
                                            end;
                                        _ ->
                                            ?ERR("check juhe result:~p", [R]),
                                            {error, 54}
                                    end;
                                _ ->
                                    {error, 1}
                            end;
                        _ ->
                            {error, 1}
                    end;
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 1}
    end.
%% ================================================================================

%% 聚合发送短信
send_msg(Mobile, Code) ->
    Url = "http://v.juhe.cn/sms/send",
    Key = sys_env:get_env(juhemsgkey),
    TplID = sys_env:get_env(juhetpl),
    NewCode = integer_to_list(Code),
    TplVale = http_uri:encode("#code#="++NewCode),
    PropList = [
                   {mobile, Mobile},
                   {tpl_id, TplID},
                   {tpl_value,TplVale},
                   {key, Key}
               ],
    Params = sdk:format_http_get_params(PropList),
    case catch sdk:http(get, Url, Params) of
        {ok, Result} ->
            case catch rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    case lists:keyfind("error_code", 1, Json) of
                        {_, ErrorCode} ->
                            case ErrorCode of
                                0 ->
                                    ok;
                                205401 ->
                                    %% 错误的手机号码
                                    {error, 30};
                                205403 ->
                                    %% 网络错误,请重试
                                    {error, 65};
                                205404 ->
                                    %% 发送失败，具体原因请参考返回reason
%%                                    case lists:keyfind("reason", 1, Json) of
%%                                        {_, Reason} ->
%%                                            ?ERROR_MSG("juhe send msg reason:~ts", [Reason]);
%%                                        _ ->
%%                                            ok
%%                                    end,
                                    {error, 66};
                                205405 ->
                                    %% 号码异常/同一号码发送次数过于频繁
                                    {error, 51};
                                10012 ->
                                    %% 请求超过次数限制
                                    {error, 51};
                                _Err ->
                                    ?ERR("~w", [_Err]),
                                    {error, 1}
                            end;
                        _Err ->
                            ?ERR("~w", [_Err]),
                            {error, 1}
                    end;
                _ ->
                    {error, 54}
            end;
        _Err ->
            ?ERR("~w", [_Err]),
            {error, 1}
    end.

%% 礼品卡
jd_card(Price, OrderID) ->
    jd_card_logic(Price, OrderID).

do_jdc_list(Price, OrderID) ->
    case db_1:select([id, cami], jdc_list, [{val, Price}, {state, 0}]) of
        {ok, []} ->
            jd_card_logic(Price, OrderID);
        {ok, [[ID, Cami] | _]} ->
            db_1:update(jdc_list, [{state, 1}], [{id, ID}]),
            NewCami = binary_to_list(Cami),
            {ok, NewCami};
        _ ->
            jd_card_logic(Price, OrderID)
    end.


jd_card_logic(Price, OrderID) ->
    Url = "http://v.juhe.cn/giftCard/buy",
    Key = sys_env:get_env(juhekey),
    Num = 1,
    ProductID = get_jd_product_id(Price),
    OpenID = sys_env:get_env(juheopenid),
    Sign = binary_to_list(util:md5(lists:concat([OpenID, Key, Num, OrderID]))),
    PropList = [
        {key, Key},
        {num, Num},
        {productId, ProductID},
        {userOrderId, OrderID},
        {sign, Sign}
    ],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            Reply = get_gift_card(Result),
            Reply;
        {error, _Error} ->
            {error, 39}
    end.

get_gift_card(Result) ->
    case rfc4627:decode(Result) of
        {ok, {obj, Json}, _} ->
            case lists:keyfind("error_code", 1, Json) of
                {_, ErrorCode} ->
                    case ErrorCode of
                        0 ->
                            [_, {_, {obj, [_,_,_,_,Cards|_]}}, _] = Json,
                            {_, [{obj, [_, {_, Pws}, _]}]} = Cards,
                            Text = base64:decode(Pws),
                            case catch crypto:block_decrypt(des_ecb, sys_env:get_env(juheacc), Text) of
                                <<X:19/binary, _/binary>> ->
                                    {ok, erlang:binary_to_list(X)};
                                _ ->
                                    false
                            end;
                        223102 ->
                            {error, 27};
                        223103 ->
                            {error, 27};
                        _ ->
                            case lists:keyfind("reason", 1, Json) of
                                {_, Reason} ->
                                    ?ERR("jdc reason :~ts", [Reason]);
                                _ ->
                                    ok
                            end,
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.

get_jd_product_id(Price) ->
    case Price of
        50 ->
            "200025";
        100 ->
            "200026";
        200 ->
            "200027";
        300 ->
            "200028";
        500 ->
            "200029";
        _ ->
            ""
    end.

%%get_msg_result(Result) ->
%%    {ok, {obj, Json}, _} = rfc4627:decode(Result),
%%    case lists:keyfind("error_code", 1, Json) of
%%        {_, ErrorCode} ->
%%            case ErrorCode of
%%                0 ->
%%                    ok;
%%                _ ->
%%                    ?ERROR_MSG("ErrorCode = ~p~n", [ErrorCode]),
%%                    false
%%            end
%%    end.


%% 根据订单号查询京东卡详情
get_jdk_info(OrderID) ->
    Url = "http://v.juhe.cn/giftCard/detail",
    Key = logic_mgr:get_juhejdkkey(),
%%    Key = "02814a6a6d76ab8b41149e393b5b0971",
    OpenID = logic_mgr:get_juheopenid(),
%%    OpenID = "JHd187f25bcb6a85422e0e844002f9f858",
    Sign = u_md5:encode(lists:concat([OpenID, Key, OrderID])),
    PropList = [
        {key, Key},
        {userOrderId, OrderID},
        {sign, Sign}
    ],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            case rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    case lists:keyfind("error_code", 1, Json) of
                        {_, ErrorCode} ->
                            case ErrorCode of
                                0 ->
                                    case lists:keyfind("result", 1, Json) of
                                        {_, {obj, Detail}} ->
                                            case lists:keyfind("cards", 1, Detail) of
                                                {_, [{obj, JdkInfo}]} ->
                                                    case lists:keyfind("cardPws", 1, JdkInfo) of
                                                        {_, CardPws} ->
                                                            Account = logic_mgr:get_juheacc(),
%%                                                            Account = "mmmabcd0",
                                                            Text = base64:decode(CardPws),
                                                            case catch crypto:block_decrypt(des_ecb, Account, Text) of
                                                                <<X:19/binary, _/binary>> ->
                                                                    NewVal =
                                                                        case lists:keyfind("productId", 1, Detail) of
                                                                            {_, PID} ->
                                                                                case PID of
                                                                                    <<"200025">> ->
                                                                                        50;
                                                                                    <<"200026">> ->
                                                                                        100;
                                                                                    <<"200027">> ->
                                                                                        200;
                                                                                    <<"200028">> ->
                                                                                        300;
                                                                                    <<"200029">> ->
                                                                                        500;
                                                                                    _ ->
                                                                                        ?ERR("Pid:~p", [PID]),
                                                                                        0
                                                                                end;
                                                                            _ ->
                                                                                0
                                                                        end,
                                                                    {ok, erlang:binary_to_list(X)};
                                                                _ ->
                                                                    {error, 1}
                                                            end;
                                                        _ ->
                                                            {error, 1}
                                                    end;
                                                _ ->
                                                    {error, 1}
                                            end;
                                        _ ->
                                            {error, 1}
                                    end;
                                _ ->
                                    {error, 1}
                            end;
                        _ ->
                            {error, 1}
                    end;
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 1}
    end.

%% 获取q币商品列表
get_qb_list() ->
    Url = "http://v.juhe.cn/giftCard/products",
    Key = "ae02449fa59b713e8927ed270998cbfd",
    Params = sdk:format_http_get_params([{key, Key}]),
    case catch sdk:http(post, Url, Params) of
        {ok, Result} ->
            case rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    case lists:keyfind("error_code", 1, Json) of
                        {_, ErrorCode} ->
                            case ErrorCode of
                                0 ->
                                    case lists:keyfind("result", 1, Json) of
                                        {_, List} ->
                                            do_list(List)
                                    end;
                                _ ->
                                    case lists:keyfind("reason", 1, Json) of
                                        {_, Reason} ->
                                            ?ERR("JUHE QB ERROR REASON:~ts", [Reason]),
                                            {error, 1};
                                        _ ->
                                            {error, 1}
                                    end
                            end;
                        _ ->
                            {error, 1}
                    end
            end;
        _ ->
            {error, 1}
    end.

do_list(List) ->
    [io:format("~ts:~ts~n", [Id, Name])||{_, [{_, Id}, {_, Name}, _, _]} <-List].




%% 购买Q币
get_qb(ProID, OrderID, QQ) ->
    Url = "http://v.juhe.cn/tencent/onlineorder",
    Num = 1,
    Key = "578b8e634e0dc9c7e984ac28443a87f6",
    Acc = "ctit0511",
    Sign = u_md5:encode(lists:concat([ProID, Num, OrderID, QQ, Key, Acc])),
    List = [
        {uorderid, OrderID},
        {proid, ProID},
        {game_userid, QQ},
        {nums, Num},
        {key, Key},
        {sign, Sign}
    ],
    Params = sdk:format_http_get_params(List),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            case rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    ?ERR("Json:~p", [Json]),
                    case lists:keyfind("error_code", 1, Json) of
                        {_, ErrorCode} ->
                            case ErrorCode of
                                0 ->
                                    case lists:keyfind("result", 1, Json) of
                                        {_, {obj, Detail}} ->
                                            case lists:keyfind("game_state", 1, Detail) of
                                                {_, Status} ->
                                                    Status;
                                                _ ->
                                                    {error, 1}
                                            end;
                                        _ ->
                                            {error, 1}
                                    end;
                                210808 ->
                                    %% 余额不足
                                    {error, 39};
                                _ ->
                                    {error, 1}
                            end;
                        _ ->
                            {error, 1}
                    end;
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 1}
    end.

get_qb_state(OrderID) ->
    Url = "http://v.juhe.cn/tencent/ordersta",
    Key = "578b8e634e0dc9c7e984ac28443a87f6",
    List = [
        {orderid, OrderID},
        {key, Key}
    ],
    Params = sdk:format_http_get_params(List),
    case sdk:http(post, Url, Params) of
        {ok, Result} ->
            case rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    ?ERR("Json:~p", [Json]),
                    case lists:keyfind("error_code", 1, Json) of
                        {_, ErrorCode} ->
                            case ErrorCode of
                                0 ->
                                    case lists:keyfind("result", 1, Json) of
                                        {_, {obj, Info}} ->
                                            case lists:keyfind("game_state", 1, Info) of
                                                {_, State} when is_binary(State)->
                                                    Status = binary_to_integer(State),
                                                    {ok, Status};
                                                _ ->
                                                    {error, 1}
                                            end;
                                        _ ->
                                            {error, 1}
                                    end;
                                210801 ->
                                    %% 产品ID不存在/暂不支持购买
                                    {error, 39};
                                210811 ->
                                    %% 错误的订单号/订单号不存在
                                    {error, 60};
                                210808 ->
                                    %% 账户余额不足
                                    {error, 39};
                                _ ->
                                    {error, 1}
                            end;
                        _ ->
                            {error, 1}
                    end;
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 1}
    end.



%% ================================================================================
%%    alarm:send_juhe(ErrorCode).
%% test juhe 17d0f8594bc6a23d1068b3d7caad4ee0 问答机器人

robot_reply() ->
    URL = "http://op.juhe.cn/robot/code",
    AppKey = "17d0f8594bc6a23d1068b3d7caad4ee0",

    PropList = [
        {key, AppKey}
    ],

    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, URL, Params) of
        {ok, Result} ->
            Result;
        {error, _Error} ->
            {error, 1}
    end.

robot(Info) ->
    URL = "http://op.juhe.cn/robot/index",
    AppKey = "17d0f8594bc6a23d1068b3d7caad4ee0",
    PropList = [
        {key, AppKey},
        {info, binary_to_list(transform:characters_to_binary(Info))}
    ],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(post, URL, Params) of
        {ok, Result} ->
            get_robot_reply(Result);
        {error, _Error} ->
            {error, 1};
        _ ->
            {error, 1}
    end.

get_robot_reply(Result) ->
    case catch rfc4627:decode(Result) of
        {ok, {obj, Json}, _} ->
            case lists:keyfind("error_code", 1, Json) of
                {_, ErrorCode} ->
                    case ErrorCode of
                        0 ->
                            case lists:keyfind("result", 1, Json) of
                                {_, Info} ->
                                    {obj, [_, {_, Reply}]} = Info,
                                    {ok, Reply};
                                _ ->
                                    {error, 1}
                            end;
                        _ ->
                            {error, 1}
                    end;
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 1}
    end.


stock() ->
    Url = "http://web.juhe.cn:8080/finance/stock/hs",
    AppKey = "f6f28327524a6d31b51aef658f659c4a",
    PropList = [
        {gid, ""},
        {key, AppKey},
        {type, 0}],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(get, Url, Params) of
        {ok, Result} ->
            case catch rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    case lists:keyfind("error_code", 1, Json) of
                        {_, ErrorCode} ->
                            case ErrorCode of
                                0 ->
                                    case lists:keyfind("result", 1, Json) of
                                        {_, {obj, Info}} ->
                                            {_, NowPri} = lists:keyfind("nowpri", 1, Info),
                                            {_, Time} = lists:keyfind("time", 1, Info),
                                            {NowPri, Time};
                                        _ ->
                                            {error, 1}
                                    end;
                                _ ->
                                    {error, 1}
                            end;
                        _ ->
                            {error, 1}
                    end;
                _ ->
                    {error, 1}
            end;
        {error, _Error} ->
            {error, 1}
    end.


io_flows() ->
    {ok, {L1, L2, L3}} = get_flow_products(),
    L = L1++L2++L3,
    Num = erlang:length(L),
    List = do_io_flows(L, []),
    Format = lists:concat(["产品:~ts  价格:~p~n" || X <- lists:seq(1, Num)]),
%%    {ok, File} = file:open("whn/flows.text", [write]),
    io:format(Format, List).

do_io_flows([], R) ->lists:reverse(R);
do_io_flows([{A,B} | T], R) ->
    do_io_flows(T, [B,transform:characters_to_binary(A) | R]).
get_flow_products() ->
    Key = "650d63f6d6bc19fe6319d2a01693e172",
    Url = "http://v.juhe.cn/flow/list",
    PropList = [{key, Key}],
    Params = sdk:format_http_get_params(PropList),
    case sdk:http(get, Url, Params) of
        {ok, Result} ->
            case rfc4627:decode(Result) of
                {ok, {obj, Json}, _} ->
                    case lists:keyfind("error_code", 1, Json) of
                        {_, 0} ->
                            case lists:keyfind("result", 1, Json) of
                                {_, [{obj, LT}, {obj, YD}, {obj, DX}]} ->
                                    {ok, {get_flows_info(LT),get_flows_info(YD),get_flows_info(DX)}};
                                _ ->
                                    {error, 1}
                            end;
                        _ ->
                            {error, 1}
                    end;
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 1}
    end.

get_flows_info(Info) ->
    case lists:keyfind("companytype", 1, Info) of
        {_, <<"1">>} ->
            case lists:keyfind("flows", 1, Info) of
                {_, Flows} ->
                    do_flows(Flows, [], "联通流量");
                _ ->
                    []
            end;
        {_, <<"2">>} ->
            case lists:keyfind("flows", 1, Info) of
                {_, Flows} ->
                    do_flows(Flows, [], "移动流量");
                _ ->
                    []
            end;
        {_, <<"3">>} ->
            case lists:keyfind("flows", 1, Info) of
                {_, Flows} ->
                    do_flows(Flows, [], "电信流量");
                _ ->
                    []
            end;
        _ ->
            []
    end.

do_flows([], R, _Name) ->
    R;
do_flows([{obj, [_,{"p", P},{"v",V},{"inprice",InPrice}]} | T], R, Name) ->
    PP = erlang:binary_to_list(P),
    Price = erlang:round(erlang:binary_to_float(InPrice)),
    do_flows(T, [{PP++Name, Price} | R], Name).
