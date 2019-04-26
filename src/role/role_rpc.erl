%%----------------------------------------------------
%% @doc 人物协议处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_rpc).
-export([
        handle/3

    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

-define(mid_act_start, date:datetime_to_seconds({2018, 9, 21, 0, 0 , 0})).
-define(mid_act_end, date:datetime_to_seconds({2018, 10, 8, 0, 0 , 0})).

%% 获取背包信息
handle(1102, _Data, Role) ->
    List = role_lib:get_bag_info(Role),
    {reply, #m_1102_toc{list = List}};

%% 实名认证
handle(1103, #m_1103_tos{id = Id, name = Name}, Role = #role{id_card = OldId}) ->
    case OldId =:= "" of
        true ->
            case catch role_lib:check_id_card(Id) of
                true ->
                    Add = 5000,
                    {ok, NewRole} = role_lib:do_add_coin(Role, Add),
                    account_mgr:output(?id_card_coin, Add),
                    {ok, #m_1103_toc{coin = Add}, NewRole#role{id_card = Id, true_name = Name}};
                _ ->
                    {false, ?error_act}
            end;
        _ ->
            {false, ?error_act}
    end;

%% 使用道具
handle(1104, #m_1104_tos{type = Type, num = Num}, Role) ->
    case role_lib:use_item(Role, Type, Num) of
        {ok, Coin, NewRole} ->
            {ok, #m_1104_toc{coin = Coin}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 使用喇叭
handle(1105, #m_1105_tos{msg = Msg}, Role) ->
    case role_lib:use_trumpet(Role, Msg) of
        {ok, NewRole} ->
            {ok, #m_1105_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 话费兑换123
handle(1107, #m_1107_tos{type = Type, phone = Phone}, Role = #role{role_id = RoleId, exchange = Exchange, channel = Channel}) ->
    case lists:member(RoleId, sys_env:get_env(robot)) of
        false ->
            case role_lib:do_cost(Role, [{tel_fare, 500}]) of
                {ok, NewRole} ->
                    case Type of
                        1 ->
                            Now = date:unixtime(),
                            OrderID = lists:concat([jd, RoleId, Now]),
                            Price = 50,
                            case lib_juhe:jd_card(Price, OrderID) of
                                {ok, Cami} ->
                                    mail_mgr:send(0, RoleId, "兑换好礼", util:fbin("恭喜成功兑换~w京东礼品卡:~n ~ts", [Price, Cami]), [], Now),
                                    log_db:log(jd_card, insert, [RoleId, 4, 500, Cami, Price, Now]),
                                    NewRole1 = role_lib:do_exchange_log(NewRole, {tel_fare, 500, jd_card, 50, date:unixtime()}),
                                    case Channel of
                                        0 -> ok;
                                        _ ->
                                            db:exec("insert into channel_exchange_log(channel_id, role_id, exchange, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleId, Price * 100, 2, date:unixtime()])
                                    end,
                                    {ok, #m_1107_toc{}, NewRole1#role{exchange = Exchange + Price * 100}};
                                _R -> 
                                    {false, ?error_busy}
                            end;
                        3 ->
                            {ok, NewRole1} = role_lib:do_add(NewRole, [{lollipop, 1}]),
                            NewRole2 = role_lib:do_exchange_log(NewRole1, {tel_fare, 500, lollipop, 1, date:unixtime()}),
                            {ok, #m_1107_toc{}, NewRole2};
                        2 ->
                            case lib_juhe:check_phone(Phone, 50) of
                                true ->
                                    OrderID = lists:concat([tel, RoleId, date:unixtime()]),
                                    Price = 50,
                                    case lib_juhe:direct_recharge(Phone, Price, OrderID) of
                                        true ->
                                            NewRole1 = role_lib:do_exchange_log(NewRole, {tel_fare, 500, tel_fare, 50, date:unixtime()}),
                                            case Channel of
                                                0 -> ok;
                                                _ ->
                                                    db:exec("insert into channel_exchange_log(channel_id, role_id, exchange, type, time) value(?, ?, ?, ?, ?)", [Channel, RoleId, Price * 100, 1, date:unixtime()])
                                            end,
                                            log_db:log(phone_card, insert, [RoleId, 4, 500, Phone, 50, date:unixtime()]),
                                            {ok, #m_1107_toc{}, NewRole1#role{exchange = Exchange + 5000}};
                                        _ ->
                                            {false, ?error_busy}
                                    end;
                                _ ->
                                    {false, ?error_phone}
                            end
                    end;
                {false, Reason} -> {false, Reason}
            end;
        true -> ok
    end;

%% 使用棒棒糖
handle(1108, #m_1108_tos{num = Num, type = Type}, Role) ->
    case role_lib:do_cost(Role, [{Type, Num}]) of
        {ok, NewRole} ->
            Add = case Type of
                lollipop -> Num * 200 * 10000;
                candy -> Num * 4 * 10000;
                lolly -> Num * 40 * 10000;
                xin_card -> Num * 4 * 10000;
                nian_card -> Num * 4 * 10000;
                kuai_card -> Num * 4 * 10000;
                le_card -> Num * 4 * 10000;
                active_card -> Num * 1000;
                _ -> 0
            end,
            {ok, NewRole1} = role_lib:do_add_coin(NewRole, Add),
            NewRole2 = role_lib:do_use_log(NewRole1, {Type, Num, coin, Add, date:unixtime()}),
            {ok, #m_1108_toc{coin = Add}, NewRole2};
        {false, Reason} -> {false, Reason}
    end;

%% 赠送棒棒糖
handle(1109, #m_1109_tos{num = Num, role_id = RoleId1, type = Type}, Role = #role{vip = Vip, role_id = RoleId, name = Name}) ->
    #vip_welfare{send = Send} = vip:get_vip_welfare(Vip),
    Value = role_lib:get_value(Role, ?daily_send),
    case lists:member(RoleId, sys_env:get_env(robot)) of
        true -> ok;
        _ ->
            case Value >= Send of
                _ ->
                    case role_lib:do_cost(Role, [{Type, Num}]) of
                        {ok, NewRole} ->
                            ItemName = case Type of
                                lollipop -> "波板糖";
                                lolly -> "棒棒糖";
                                candy -> "糖果";
                                xin_card -> "\"新\"卡";
                                nian_card -> "\"年\"卡";
                                kuai_card -> "\"快\"卡";
                                le_card -> "\"乐\"卡"
                            end,
                            mail_mgr:send(RoleId, RoleId1, "好友礼物", util:fbin("尊敬的猎手:\n   您好，您的好友[~ts]为你们之间的友谊更加裸露，送给您~w个~ts！", [Name, Num, ItemName]), [#p_assets{type = Type, num = Num}], date:unixtime()),
                            NewRole1 = role_lib:do_send_log(NewRole, {RoleId1, Type, Num, date:unixtime()}),
                            NewRole2 = role_lib:add_value(NewRole1, ?daily_send),
                            {ok, #m_1109_toc{}, NewRole2};
                        {false, Reason} -> {false, Reason}
                    end;
                true -> {false, ?error_num}
            end
    end;

%% 棒棒糖合成
handle(1110, #m_1110_tos{type = Type, num = Num}, Role = #role{}) ->
    {Type1, Need} = case Type of
        candy -> {lolly, 10};
        lolly -> {lollipop, 5}
    end,
    case role_lib:do_cost(Role, [{Type, Num * Need}]) of
        {ok, NewRole} ->
            {ok, NewRole1} = role_lib:do_add(NewRole, [{Type1, Num}]),
            {ok, #m_1110_toc{}, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end;




%% 领取贵族礼包
handle(1111, _, Role = #role{gift = Gift = #role_gift{end_time = Time, reward_time = Reward}}) ->
    Now = date:unixtime(),
    case Now >= Time of
        true ->
            {false, ?error_act};
        _ ->
            case date:is_same_day(Now, Reward) of
                true ->
                    {false, ?error_act};
                _ ->
                    Items = [{coin, 40000}, {ice, 2}, {locking, 5}, {gold, 20}],
                    {ok, NewRole} = role_lib:do_add(Role, Items),
                    account_mgr:output(?charge_gift, 40000),
                    {ok, #m_1111_toc{list = [#p_assets{type = Type, num = Value}||{Type, Value} <-Items]}, NewRole#role{gift = Gift#role_gift{reward_time = Now}}}
            end
    end;

%% 获取贵族礼包信息
handle(1112, _, _Role = #role{gift = #role_gift{buy_time = BuyTime, end_time = EndTime, reward_time = RewardTime}}) ->
    Now = date:unixtime(),
    Reward = case date:is_same_day(Now, RewardTime) of
        true -> 1;
        _ -> 0
    end,
    {reply, #m_1112_toc{buy_time = BuyTime, end_time = EndTime, reward = Reward}};

%% 获取赠送道具日志
handle(1113, _, _Role = #role{send_log = LogList}) ->
    List = [#p_send_log{role_id = RoleId, type = Type, num = Num, time = Time}||{RoleId, Type, Num, Time} <-LogList],
    {reply, #m_1113_toc{list = List}};

%% 获取使用道具日志
handle(1114, _, _Role = #role{use_log = LogList}) ->
    List = [#p_use_log{type = Type, num = Num, type1 = Type1, num1 = Num1, time = Time}||{Type, Num, Type1, Num1, Time} <-LogList],
    {reply, #m_1114_toc{list = List}};

%% 获取话费兑换日志
handle(1115, _, _Role = #role{exchange_log = LogList}) ->
    List = [#p_use_log{type = Type, num = Num, type1 = Type1, num1 = Num1, time = Time}||{Type, Num, Type1, Num1, Time} <-LogList],
    {reply, #m_1115_toc{list = List}};

%% 查看玩家信息
handle(1116, #m_1116_tos{id = RoleId}, _Role) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{name = Name, sign = Sign}} ->
            {reply, #m_1116_toc{name = Name, sign = Sign}};
        _ ->
            case role_data:get_role_from_dets(RoleId) of
                {ok, #role{name = Name, sign = Sign}} ->
                    {reply, #m_1116_toc{name = Name, sign = Sign}};
                _ ->
                    {false, ?error_role_exit}
            end
    end;

%% 登陆奖励信息 %% 新手指引礼包领取后第二天才能领取
handle(1117, _, Role = #role{guide_gift = Day, guide_gift_time = Time}) ->
    Now = date:unixtime(),
    Num = case Day of
        0 ->
            1;
        1 ->
            case date:is_same_day(Time, Now) of
                true ->
                    1;
                _ ->
                    role_lib:get_value(Role, ?daily_login)
            end;
        _ ->
            role_lib:get_value(Role, ?daily_login)
    end,
    {reply, #m_1117_toc{num = Num}};

%% 登陆奖励抽奖
handle(1118, _, Role = #role{guide_gift = Day, guide_gift_time = Time}) ->
    Now = date:unixtime(),
    case Day of
        0 ->
            {false, ?error_act};
        1 ->
            case date:is_same_day(Time, Now) of
                true ->
                    {false, ?error_act};
                _ ->
                    case role_lib:do_login_reward(Role) of
                        {ok, Type, Num, NewRole} ->
                            account_mgr:output(?login_coin, [{Type, Num}]),
                            {ok, #m_1118_toc{type = Type, num = Num}, NewRole};
                        {false, Reason} ->
                            {false, Reason}
                    end
            end;
        _ ->
            case role_lib:do_login_reward(Role) of
                {ok, Type, Num, NewRole} ->
                    account_mgr:output(?login_coin, [{Type, Num}]),
                    {ok, #m_1118_toc{type = Type, num = Num}, NewRole};
                {false, Reason} ->
                    {false, Reason}
            end
    end;

%% 获取救济金次数 
handle(1119, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_alms),
    {reply, #m_1119_toc{num = Num}};

%% 领取vip救济金奖励
handle(1120, _, Role) ->
    case role_lib:do_alms_reward(Role) of
        {ok, Type, Num, NewRole} ->
            account_mgr:output(?vip_alms_coin, Num),
            {ok, #m_1120_toc{type = Type, num = Num}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取赠送次数 
handle(1121, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_send),
    {reply, #m_1121_toc{num = Num}};

%% 充值接口
handle(1122, #m_1122_tos{type = 3}, _Role = #role{first_gift = 1}) ->
    {false, ?error_first_gift};
handle(1122, #m_1122_tos{type = Type, num = Num, charge_type = ChargeType, return_url = ReturnUrl}, Role) ->
    case charge:do_charge(Role, Type, Num, ChargeType, ReturnUrl) of
        {ok, Data} ->
            {reply, #m_1122_toc{info = Data, charge_type = ChargeType}};
        {false, Reason} ->
            {false, Reason}
    end;


%% 获取vip信息
handle(1125, _, _Role = #role{vip = Vip, vip_charge = VipCharge}) ->
    {reply, #m_1125_toc{vip = Vip, charge = VipCharge}};

%% Vip登陆奖励信息
handle(1126, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_vip_login),
    {reply, #m_1126_toc{num = Num}};

%% 登陆奖励抽奖
handle(1127, _, Role) ->
    case role_lib:do_vip_login_reward(Role) of
        {ok, Type, Num, NewRole} ->
            {ok, #m_1127_toc{type = Type, num = Num}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 修改签名和名字
handle(1128, #m_1128_tos{type = Type, msg = Msg}, Role) ->
    case role_lib:set_role(Role, Type, Msg) of
        {ok, NewRole} ->
            {ok, #m_1128_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 手机获取验证码, 每天只能验证5次，每次要间隔一分钟
handle(1130, #m_1130_tos{phone = Phone}, Role = #role{phone = ""}) ->
    case erlang:length(Phone) of
        11 ->
            case role_lib:get_value(Role, ?daily_phone) >= 5 of
                true -> {false, ?error_phone_num};
                _ ->
                    Now = date:unixtime(),
                    case get(phone_code) of
                        {_, _, Time} when Time >= Now ->
                            {false, ?error_act};
                        _ ->
                            case db:get_one("select role_id from role where phone = ?", [Phone]) of
                                {ok, _RoleId} when is_integer(_RoleId) ->   %% 已经绑定
                                    {false, ?error_phone_exit};
                                _ ->
                                    Code = sys_rand:rand(1000, 9999),
                                    put(phone_code, {Code, Phone, Now + 60}),
                                    lib_juhe:send_msg(Phone, Code),
                                    NewRole = role_lib:add_value(Role, ?daily_phone),
                                    {ok, #m_1130_toc{}, NewRole}
                            end
                    end
            end;
        _ ->
            {false, ?error_phone}
    end;

%% 绑定手机
handle(1131, #m_1131_tos{code = Code}, Role) ->
    case get(phone_code) of
        {Code, Phone, _} ->
            {ok, NewRole} = role_lib:do_add_gold(Role, 2),
            role_data:save_to_db(NewRole#role{phone = Phone}),
            {ok, #m_1131_toc{type = gold, num = 2, phone = Phone}, NewRole#role{phone = Phone}};
        _ ->
            {false, ?error_phone_code}
    end;

%% 获取每日分享次数
handle(1132, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_share),
    {reply, #m_1132_toc{num = Num}};

%% 领取分享奖励
handle(1133, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_share),
    case Num =:= 0 of
        true ->
            Coin = 3000,
            {ok, NewRole} = role_lib:do_add_coin(Role, Coin),
            NewRole1 = role_lib:add_value(NewRole, ?daily_share),
            account_mgr:output(?share_coin, Coin),
            NewRole2 = task_week:handle(NewRole1, share, 1),
            {ok, #m_1133_toc{coin = Coin}, NewRole2};
        _ ->
            {false, ?error_share}
    end;

%% 获取每日救济金次数
handle(1134, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_free_alms),
    {reply, #m_1134_toc{num = Num}};

%% 领取每日救济金奖励
handle(1135, _, Role = #role{coin = Coin}) ->
    Num = role_lib:get_value(Role, ?daily_free_alms),
    case Num >= 3 of
        true -> {false, ?error_alms_num};
        _ ->
            Add = 10000,
            {ok, NewRole} = role_lib:do_add_coin(Role, Add),
            NewRole1 = role_lib:add_value(NewRole, ?daily_free_alms),
            account_mgr:output(?alms_coin, Coin),
            {ok, #m_1135_toc{coin = Add}, NewRole1}
    end;

%% 分享签名
handle(1136, #m_1136_tos{url = Url}, _) ->
    case weixin_mgr:client_sign(Url) of
        {ok, Sign, TimeStamp, NonceStr} ->
            Data = #m_1136_toc{sign = erlang:binary_to_list(Sign), timestamp = erlang:integer_to_list(TimeStamp), noncestr = erlang:integer_to_list(NonceStr)},
            {reply, Data};
        _ ->
            {false, ?error_busy}
    end;

%% 领取红包 
handle(1137, _, Role = #role{role_id = RoleId}) ->
    case lists:member(RoleId, sys_env:get_env(robot)) of
        true -> ok;
        _ ->
            case role_lib:reward_red_bag(Role) of
                {ok, NewRole} ->
                    {ok, #m_1137_toc{}, NewRole};
                {false, Reason} ->
                    {false, Reason}
            end
    end;

%% 获取今日领取红包次数
handle(1138, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_red_bag),
    Min = case setting_mgr:get(?setting_redbag) of
        {ok, Setting} -> Setting * 10;
        _ -> ?redbag_setting * 10
    end,
    {reply,  #m_1138_toc{num = Num, min = Min}};

%% 获取中秋节领奖次数
handle(1139, _, Role) ->
    Num = role_lib:get_value(Role, ?daily_mid_act),
    {reply,  #m_1139_toc{num = Num}};

%% 中秋节领奖
handle(1140, _, Role) ->
    Time = date:unixtime(),
    case Time >= ?mid_act_start andalso Time =< ?mid_act_end of
        true -> 
            case role_lib:get_value(Role, ?daily_mid_act) of
                0 -> 
                    {ok, NewRole} = role_lib:do_add_coin(Role, 1000),
                    NewRole1 = role_lib:add_value(NewRole, ?daily_mid_act),
                    {ok, #m_1140_toc{}, NewRole1};
                _ ->
                    {false, ?error_num}
            end;
        _ ->
            {false, ?error_act_time}
    end;

%% 获取好友信息
handle(1141, #m_1141_tos{num = Num, page = Page, page_num = PageNum}, _Role = #role{role_id = RoleId}) ->
    {Coin, Num1, All, List, Setting} = friend_mgr:get_friend_info(RoleId, Num, Page, PageNum),
    {reply, #m_1141_toc{coin = Coin, num = Num1, allpage = All, list = List, setting = Setting}};

%% 获取充值今日明日返利信息
handle(1142, _, _Role = #role{charge_reward_today = Today, charge_reward_tomorrow = Tomorrow}) ->
    {reply, #m_1142_toc{today = Today, tomorrow = Tomorrow}};

%% 领取充值返利
handle(1143, _, Role = #role{charge_reward_today = Today}) ->
    case Today > 0 of
        true ->
            {ok, NewRole} = role_lib:do_add_coin(Role, Today),
            {ok, #m_1143_toc{}, NewRole#role{charge_reward_today = 0}};
        _ ->
            {false, ?error_num}
    end;

%% 领取礼包码奖励
handle(1144, #m_1144_tos{code = Code}, Role = #role{gift_code = Flag}) ->
    case Flag < ?gift_code_flag of
        true ->
            case Code =:= ?gift_code of
                true ->
                    {ok, NewRole} = role_lib:do_add(Role, ?gift_code_reward),
                    account_mgr:output(?gift_code_coin, ?gift_code_reward),
                    {ok, #m_1144_toc{list = [#p_assets{type = Type, num = Num}||{Type, Num} <-?gift_code_reward]}, NewRole#role{gift_code = ?gift_code_flag}};
                _ ->
                    {false, ?error_gift_code}
            end;
        _ ->
            {false, ?error_gift_code_reward}
    end;

%% 获取聊天信息列表
handle(1145, _, _Role = #role{talk_list = List}) ->
    NewList = role_talk:to_talk_list(List, [], []),
    {reply, #m_1145_toc{list = NewList}};

%% 发送聊天
handle(1146, #m_1146_tos{role_id = ToId, message = Msg}, Role) ->
    case role_talk:talk_to_other(Role, ToId, Msg) of
        {ok, NewRole} ->
            {ok, #m_1146_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 红包抵扣充值
handle(1150, #m_1150_tos{type = Type, num = Num}, Role) ->
    case charge:redbag_to_charge(Role, Type, Num) of
        {ok, Add, NewRole} ->
            {ok, #m_1150_toc{num = Add}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 新年卡合成
handle(1151, #m_1151_tos{type = Type, num = Num}, Role = #role{}) ->
    NeedList = case Type of
        lollipop -> [{xin_card, Num}, {nian_card, Num}, {kuai_card, Num}, {le_card, Num}]
    end,
    case role_lib:do_cost(Role, NeedList) of
        {ok, NewRole} ->
            {ok, NewRole1} = role_lib:do_add(NewRole, [{Type, Num}]),
            {ok, #m_1151_toc{}, NewRole1};
        {false, Reason} ->
            {false, Reason}
    end;

%% 选择击打特效
handle(1152, #m_1152_tos{vip = Vip}, Role = #role{}) ->
    {ok, #m_1152_toc{}, Role#role{vip_effect = Vip}};

%% 获取成就信息
handle(1154, _, Role) ->
    List = role_achievement:get_info(Role),
    {reply, #m_1154_toc{list = List}};

%% 领取成就奖励
handle(1155, #m_1155_tos{type = Type, id = Id}, Role) ->
    case role_achievement:reward(Role, Type, Id) of
        {ok, NewRole} ->
            {ok, #m_1155_toc{type = Type, id = Id}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取每周任务
handle(1157, _, Role) ->
    List = task_week:get_info(Role),
    {reply, #m_1157_toc{list = List}};

%% 领取每周任务奖励
handle(1158, #m_1158_tos{id = Id}, Role) ->
    case task_week:get_reward(Role, Id) of
        {ok, NewRole} ->
            {ok, #m_1158_toc{id = Id}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取每日签到信息
handle(1159, _, _Role = #role{daily_sign = {Day, Time}}) ->
    Now = date:unixtime(),
    {NewDay, Flag} = case date:is_same_day(Time, Now) of
        true ->  {Day, 1}; 
        _ ->
            Day1 = case Day of
                7 -> 0;
                _ -> Day
            end,
            {Day1, 0}
    end,
    {reply, #m_1159_toc{day = NewDay, flag = Flag}};

%% 领取每日签到
handle(1160, _, Role = #role{daily_sign = {Day, Time}}) ->
    Now = date:unixtime(),
    case date:is_same_day(Now, Time) of
        true -> {false, ?error_act};
        _ ->
            NewDay = case Day of
                7 -> 1;
                _ -> Day + 1
            end,
            {Reward} = mission_sign_setting:get_data(NewDay),
            {ok, NewRole} = role_lib:do_add(Role, Reward),
            NewRole1 = task_week:handle(NewRole, sign, 1),
            {ok, #m_1160_toc{day = NewDay}, NewRole1#role{daily_sign = {NewDay, Now}}}
    end;

%% 领取关注
handle(1161, _, Role = #role{subscribe = Sub, subscribe_reward = Reward}) ->
    case Reward =:= 0 andalso Sub =:= 1 of
        true ->
            {ok, NewRole} = role_lib:add(Role, [{coin, 1000}, {gold, 10}, {ice, 5}, {locking, 5}]),
            {ok, #m_1161_toc{}, NewRole#role{subscribe_reward = 1}};
        _ ->
            {false, ?error_act}
    end;


%% 前端报错的错误日志
handle(1197, #m_1197_tos{msg = Msg}, #role{role_id = RoleId, name = Name}) ->
    case get(client_error_log) of
        Msg -> ok;
        _ ->
            put(client_error_log, Msg),
            log_db:log(client_error_log, insert, [RoleId, Name, Msg, date:unixtime()])
    end,
    {reply, #m_1197_toc{}};

%% 使用gm命令
%% 充值
handle(1198, #m_1198_tos{type = Type, num = Num}, Role = #role{role_id = RoleId}) ->
    Now = date:unixtime(),
    N = sys_rand:rand(1000, 9999),
    Id = lists:concat([RoleId, N, Now]),
    case db:exec("insert into charge_log (id, role_id, charge_rmb, type, time, status) values(?, ?, ?, ?, ?, ?)", [Id, RoleId, Num, Type, Now, 0]) of
        ok ->
            {ok, NewRole} = charge:charge_callback(Role, {Id, Type, Num}),
            {ok, #m_1198_toc{}, NewRole};
        _ -> {false, ?error_busy}
    end;
%%%% 获得数据
%%handle(1199, #m_1199_tos{type = Type, num = Num}, Role) ->
%%    {ok, NewRole} = role_lib:do_add(Role, [{Type, Num}]),
%%    {ok, #m_1199_toc{}, NewRole};
handle(_Cmd, _Data, _Role) ->
    ?ERR("错误的协议数据cmd:~w,data:~w", [_Cmd, _Data]),
    ok.
%%---------------------------------
%% internal function
%%---------------------------------


