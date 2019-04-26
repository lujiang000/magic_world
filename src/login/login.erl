%%----------------------------------------------------
%% @doc 登陆处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(login).
-export([
        weixin_login/2
       ,account_login/2
       ,screat_login/3
       ,phone_login/2
       ,phone_screat_login/2
       ,create/5
       ,id_login/2
    ]).


-include("common.hrl").
-include("role.hrl").
-include("conn.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

-define(default_icon_list, [
    "http://www.zhongchuanxinxi.com/baping/public/user/ZmAA5a5c743b9d5bd6445_thumb.png"
    ,"baping/public/user/epUm5a638dc7240f41476_thumb.png"
    ,"http://wx.qlogo.cn/mmopen/vi_32/Q0j4TwGTfTLxAy8ugneYiccpB9ThqLMDBIJZ2ticdMMEKmPq1SAEdnLx7nxo22GcbmseTc2rAosDpVmAvfYcPe8A/0"
    ,"http://wx.qlogo.cn/mmopen/vi_32/DYAIOgq83eoiaE7OvwXbU80WFWAs3FN9VaY21rcCdg1tcD7FC2PzGeeMNQr8HK7OgUbiaymTicxmDlpCFm7yfubAA/0"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048355&di=8d516bede6e3eb3751aeb9f32a4e0fe1&imgtype=0&src=http%3A%2F%2Fimg.18183.com%2Fuploads%2Fallimg%2F183%2F180120%2F201-1P1201A533.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048355&di=8d516bede6e3eb3751aeb9f32a4e0fe1&imgtype=0&src=http%3A%2F%2Fimg.18183.com%2Fuploads%2Fallimg%2F183%2F180120%2F201-1P1201A533.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048354&di=9685ea2fb712426fd1cb279e85da0993&imgtype=0&src=http%3A%2F%2Fimg.9553.com%2Fuploadfile%2F2018%2F0119%2F20180119024738725.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048354&di=fde76d6264810935d81667d10a139154&imgtype=0&src=http%3A%2F%2Fupload.techweb.com.cn%2Fs%2F640%2F2018%2F0122%2F1516603531513.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048353&di=1f391b732c65ffc84cff801f4c5eb08e&imgtype=0&src=http%3A%2F%2Fdown.52pk.com%2Fuploads%2F180119%2F5010_094843_7422.jpg"
]).

%% 微信登陆
weixin_login({Code, ParentID, RedCode, PayCode, Channel, RoleId, Flag}, State) ->
    case get_weixin_OpendId(Code) of
        {OpenID, NickName, Icon, Sex, Ss} ->
             case get_weixin_RedId(RedCode) of
                 {ok, RedId} ->
                     case get_weixin_PayId(PayCode) of
                         {ok, PayId} ->
                             do_login({OpenID, NickName, Icon, Sex, ParentID, RedId, PayId, Channel, RoleId, Flag, Ss}, State);
                         _ ->
                             {false, ?error_busy}
                    end;
                _ ->
                    {false, ?error_busy}
            end;
        _ ->
            {false, ?error_busy}
    end.

%% 获取微信openid
get_weixin_OpendId(Code) ->
    PropList = [
        {appid, sys_env:get_env(loginAppId)},
        {secret, sys_env:get_env(loginAppSecret)},
        {code, Code},
        {grant_type, authorization_code}
    ],
    Params = string:join([atom_to_list(Key) ++ "=" ++ util:to_list(Val) || {Key, Val} <- PropList], "&"),
    Url = "https://api.weixin.qq.com/sns/oauth2/access_token",
    Headers = [{"content-type", "ext/xml;charset=utf-8"}],
    case httpc:request(get, {Url++"?"++Params, Headers}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _List, Result}} ->
            case catch json:decode(erlang:list_to_binary(Result), [{object_format,proplist}]) of
                Json when is_list(Json) ->
                    case lists:keyfind(<<"access_token">>, 1, Json) of
                        {_, AccessToken} ->
                            case lists:keyfind(<<"openid">>, 1, Json) of
                                {_, OpenID} ->
                                    get_weixin_info(AccessToken, OpenID);
                                _ ->
                                    ""
                            end;
                        _ ->
                            ""
                    end;
                _ErrorJson ->
                    ""
            end;
        _ ->
            ""
    end.

%% 获取支付id
get_weixin_PayId([]) ->
    ?ERR("支付code为空", []),
    "";
get_weixin_PayId(Code) ->
    AppID = sys_env:get_env(payAppId),
    AppSecret = sys_env:get_env(payAppSecret),
    PropList = [
        {appid, AppID},
        {secret, AppSecret},
        {code, Code},
        {grant_type, authorization_code}
    ],
    Params = string:join([atom_to_list(Key) ++ "=" ++ util:to_list(Val) || {Key, Val} <- PropList], "&"),
    Url = "https://api.weixin.qq.com/sns/oauth2/access_token",
    Headers = [{"content-type", "ext/xml;charset=utf-8"}],
    case httpc:request(get, {Url++"?"++Params, Headers}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _List, Result}} ->
            case catch json:decode(erlang:list_to_binary(Result), [{object_format,proplist}]) of
                Json when is_list(Json) ->
                    case lists:keyfind(<<"openid">>, 1, Json) of
                        {_, OpenID} ->
                            {ok, OpenID};
                        _ ->
                            ?ERR("支付code授权失败：~w", [Json]),
                            ""
                    end;
                _ErrorJson ->
                    ?ERR("支付code授权失败", []),
                    ""
            end;
        _ ->
            ?ERR("支付code授权失败", []),
            ""
    end.

%% 获取发红包id
get_weixin_RedId([]) -> 
    ?ERR("红包code为空", []),
    "";
get_weixin_RedId(Code) ->
    AppID = sys_env:get_env(redAppId),
    AppSecret = sys_env:get_env(redAppSecret),
    PropList = [
        {appid, AppID},
        {secret, AppSecret},
        {code, Code},
        {grant_type, authorization_code}
    ],
    Params = string:join([atom_to_list(Key) ++ "=" ++ util:to_list(Val) || {Key, Val} <- PropList], "&"),
    Url = "https://api.weixin.qq.com/sns/oauth2/access_token",
    Headers = [{"content-type", "ext/xml;charset=utf-8"}],
    case httpc:request(get, {Url++"?"++Params, Headers}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _List, Result}} ->
            case catch json:decode(erlang:list_to_binary(Result), [{object_format,proplist}]) of
                Json when is_list(Json) ->
                    case lists:keyfind(<<"openid">>, 1, Json) of
                        {_, OpenID} ->
                            {ok, OpenID};
                        _ ->
                            ?ERR("红包code授权失败:~w", [Json]),
                            ""
                    end;
                _ErrorJson ->
                    ?ERR("红包code授权失败", []),
                    ""
            end;
        _ ->
            ?ERR("红包code授权失败", []),
            ""
    end.

%% 通过AceessToken和OpenID获取微信玩家详细信息 {OpenID, NickName, Icon, Sex}
get_weixin_info(AccessToken, OpenID) ->
    %%Url = "https://api.weixin.qq.com/sns/userinfo",
    Url = "https://api.weixin.qq.com/cgi-bin/user/info",
    PropList = [
        {access_token, AccessToken},
        {openid, OpenID},
        {lang, zh_CN}
    ],
    Params = string:join([atom_to_list(Key) ++ "=" ++ util:to_list(Val) || {Key, Val} <- PropList], "&"),
    Headers = [{"content-type", "ext/xml;charset=utf-8"}],
    case httpc:request(get, {Url++"?"++Params, Headers}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _List, Result}} ->
            case catch json:decode(erlang:list_to_binary(Result), [{object_format,proplist}]) of
                Json when is_list(Json) ->
                    ?ERR("", [Json]),
                    case lists:keyfind(<<"nickname">>, 1, Json) of
                        {_, NickName} ->
                            case lists:keyfind(<<"headimgurl">>, 1, Json) of
                                {_, Icon} ->
                                    case lists:keyfind(<<"sex">>, 1, Json) of
                                        {_, Sex} ->
                                            case lists:keyfind(<<"subscribe">>, 1, Json) of
                                                {_, Ss} ->
                                                    {OpenID, NickName, to_high_definition(Icon), Sex, Ss};
                                                _ ->
                                                    ""
                                            end;
                                        _ ->
                                            ""
                                    end;
                                _ ->
                                    ""
                            end;
                        _ ->
                            ""
                    end;
                _ ->
                    ""
            end;
        _ ->
            ""
    end.

%% 把头像转换格式
to_high_definition(NickName) when is_list(NickName) ->
    case catch string:tokens(NickName, "/") of
        List when is_list(List) ->
            case lists:reverse(List) of
                [Last | L] ->
                    case Last of
                        "0" ->
                            NickName;
                        _ ->
                            List1 = lists:reverse(["0" | L]),
                            [A | L1] = List1,
                            L2 = string:join(L1, "/"),
                            erlang:list_to_binary(A++"//"++L2)
                    end;
                _ ->
                    NickName
            end;
        _ ->
            NickName
    end;
to_high_definition(NickName1) ->
    NickName = unicode:characters_to_list(NickName1),
    case catch string:tokens(NickName, "/") of
        List when is_list(List) ->
            case lists:reverse(List) of
                [Last | L] ->
                    case Last of
                        "0" ->
                            NickName1;
                        _ ->
                            List1 = lists:reverse(["0" | L]),
                            [A | L1] = List1,
                            L2 = string:join(L1, "/"),
                            erlang:list_to_binary(A++"//"++L2)
                    end;
                _ ->
                    NickName1
            end;
        _ ->
            NickName1
    end.


%% 账号登陆
account_login(Account, State) ->
    do_login({Account, lists:concat(["游客", Account]),  sys_rand:rand_list(?default_icon_list), 0, 0, "", "", 0, 0, 0, 0}, State).
    
%% 断线重连
screat_login(RoleId, Id, State = #conn{ip = Ip}) ->
    case role_black:is_black(RoleId) of
        ok ->
            case role_data:get_online_role(RoleId) of
                {ok, #online_role{pid = Pid, screat = Id}} ->    %% 在线断线重连
                    case is_process_alive(Pid) of
                        true ->
                            LoginRole = gen_server:call(Pid, {reconnect, self(), Ip}),
                            {ok, LoginRole, Id, State#conn{pid_object = Pid}};
                        _ ->
                            {false, ?error_screat}
                    end;
                _ ->
                    {false, ?error_screat}
            end;
        {StartTime, EndTime} ->
            sys_conn:pack_send(self(), 1149, #m_1149_toc{start_time = StartTime, end_time = EndTime}),
            {false, ?error_black}
    end.

%% 手机密码登陆
phone_login({Phone, PhoneScreat}, State = #conn{ip = IP}) ->
    case db:get_row("select role_id, phone_screat from role where phone = ?", [Phone]) of
        {ok, [_, undefined]} -> {false, ?error_phone_screat}; 
        {ok, [RoleId, PhoneScreat1]} ->
            case role_black:is_black(RoleId) of
                ok ->
                    case unicode:characters_to_list(PhoneScreat1) of
                        PhoneScreat ->
                            case role_data:get_online_role(RoleId) of
                                {ok, #online_role{pid = Pid, screat = Screat}} ->    %% 在线断线重连
                                    case catch  gen_server:call(Pid, {reconnect, self(), IP}) of
                                        LoginRole = #p_role_info{} ->
                                            {ok, LoginRole, Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                        _ ->
                                            ets:delete(online_role, RoleId),
                                            {false, ?error_busy}
                                    end;
                                _ ->  
                                    Screat = sys_rand:rand(10000, 999999),
                                    case role_data:get_role_from_dets(RoleId) of
                                        {ok, Term} ->
                                            case role_var:update_var(Term) of
                                                {ok, Role = #role{}} ->
                                                    {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, screat = Screat}),
                                                    {ok, role_conver:to_login_role(Role), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                                _Err ->
                                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                    {false, ?error_role_data}
                                            end;
                                        [] ->
                                            case db:get_one("select info from role where role_id = ?", [RoleId]) of
                                                {ok, Info} ->
                                                    case util:string_to_term(Info) of
                                                        {ok, Term} ->
                                                            case role_var:update_var(Term) of
                                                                {ok, Role = #role{}} ->
                                                                    {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, screat = Screat}),
                                                                    {ok, role_conver:to_login_role(Role), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                                                _Err ->
                                                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                                    {false, ?error_role_data}
                                                            end;
                                                        {error, _Err}->
                                                            ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId, _Err]),
                                                            {false, ?error_role_data}
                                                    end;
                                                _Err ->
                                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                    {false, ?error_busy}
                                            end;
                                        _Err ->
                                            {false, ?error_busy}
                                    end
                            end;
                        _ ->
                            {false, ?error_phone_screat}
                    end;
                {StartTime, EndTime} ->
                    sys_conn:pack_send(self(), 1149, #m_1149_toc{start_time = StartTime, end_time = EndTime}),
                    {false, ?error_black}
            end;
        _ ->
            {false, ?error_phone_screat}
    end.

%% 手机验证码登陆
phone_screat_login({Phone, PhoneScreat, ParentID, Channel}, State = #conn{ip = IP}) ->
    case db:get_one("select role_id from role where phone = ?", [Phone]) of
        {ok, RoleId} when is_integer(RoleId) ->
            case role_black:is_black(RoleId) of
                ok ->
                    db:exec("update role set phone_screat = ?  where role_id = ?", [PhoneScreat, RoleId]),
                    case role_data:get_online_role(RoleId) of
                        {ok, #online_role{pid = Pid, screat = Screat}} ->    %% 在线断线重连
                            case catch  gen_server:call(Pid, {reconnect, self(), IP, PhoneScreat}) of
                                LoginRole = #p_role_info{} ->
                                    {ok, LoginRole, Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                _ ->
                                    ets:delete(online_role, RoleId),
                                    {false, ?error_busy}
                            end;
                        _ ->  
                            Screat = sys_rand:rand(10000, 999999),
                            case role_data:get_role_from_dets(RoleId) of
                                {ok, Term} ->
                                    case role_var:update_var(Term) of
                                        {ok, Role = #role{}} ->
                                            {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, screat = Screat, phone_screat = PhoneScreat}),
                                            {ok, role_conver:to_login_role(Role), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                        _Err ->
                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                            {false, ?error_role_data}
                                    end;
                                [] ->
                                    case db:get_one("select info from role where role_id = ?", [RoleId]) of
                                        {ok, Info} ->
                                            case util:string_to_term(Info) of
                                                {ok, Term} ->
                                                    case role_var:update_var(Term) of
                                                        {ok, Role = #role{}} ->
                                                            {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, screat = Screat, phone_screat = PhoneScreat}),
                                                            {ok, role_conver:to_login_role(Role), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                                        _Err ->
                                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                            {false, ?error_role_data}
                                                    end;
                                                {error, _Err}->
                                                    ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId, _Err]),
                                                    {false, ?error_role_data}
                                            end;
                                        _Err ->
                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                            {false, ?error_busy}
                                    end;
                                _Err ->
                                    {false, ?error_busy}
                            end
                    end;
                {StartTime, EndTime} ->
                    sys_conn:pack_send(self(), 1149, #m_1149_toc{start_time = StartTime, end_time = EndTime}),
                    {false, ?error_black}
            end;
        {ok, undefined} ->
            Name = lists:sublist(Phone, 1, 3) ++ "****" ++ lists:sublist(Phone, 8, 4),
            create({"", Name, "", 0, ParentID, "", "", Channel, Phone, PhoneScreat}, State);
        _Err ->
            ?ERR("数据库读取玩家数据错误~w", [_Err]),
            {false, ?error_busy}
    end.





%% 账号登陆，微信登陆都走这里 微信正常登陆
do_login({OpenId, NickName, Icon, Sex, ParentID, RedId, PayId, Channel, 0, _Flag, Ss}, State = #conn{ip = IP}) ->
    case db:get_one("select role_id from role where openid = ?", [OpenId]) of
        {ok, RoleId} when is_integer(RoleId) ->   %% 老玩家
            case role_black:is_black(RoleId) of
                ok ->
                    case role_data:get_online_role(RoleId) of
                        {ok, #online_role{pid = Pid, screat = Screat}} ->    %% 在线断线重连
                            case catch  gen_server:call(Pid, {reconnect, self(), IP, OpenId, RedId, PayId, Ss}) of
                                LoginRole = #p_role_info{} ->
                                    {ok, LoginRole, Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                _ ->
                                    ets:delete(online_role, RoleId),
                                    {false, ?error_busy}
                            end;
                        _ ->  
                            Screat = sys_rand:rand(10000, 999999),
                            case role_data:get_role_from_dets(RoleId) of
                                {ok, Term} ->
                                    case role_var:update_var(Term) of
                                        {ok, Role = #role{}} ->
                                            NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                            {ok, Pid} = role:start(NewRole),
                                            {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                        _Err ->
                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                            {false, ?error_role_data}
                                    end;
                                [] ->
                                    case db:get_one("select info from role where role_id = ?", [RoleId]) of
                                        {ok, Info} ->
                                            case util:string_to_term(Info) of
                                                {ok, Term} ->
                                                    case role_var:update_var(Term) of
                                                        {ok, Role = #role{}} ->
                                                            NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                                            {ok, Pid} = role:start(NewRole),
                                                            {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                                        _Err ->
                                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                            {false, ?error_role_data}
                                                    end;
                                                {error, _Err}->
                                                    ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId, _Err]),
                                                    {false, ?error_role_data}
                                            end;
                                        _Err ->
                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                            {false, ?error_busy}
                                    end;
                                _Err ->
                                    {false, ?error_busy}
                            end
                    end;
                {StartTime, EndTime} ->
                    sys_conn:pack_send(self(), 1149, #m_1149_toc{start_time = StartTime, end_time = EndTime}),
                    {false, ?error_black}
            end;
        {ok, undefined} ->
            create({OpenId, NickName, Icon, Sex, ParentID, RedId, PayId, Channel, "", ""}, State);
        _Err ->
            ?ERR("数据库读取玩家数据错误~w", [_Err]),
            {false, ?error_busy}
    end;

%% 绑定微信号
do_login({OpenId, NickName, Icon, Sex, _ParentID, RedId, PayId, _Channel, RoleId, Flag, Ss}, State = #conn{ip = IP})  when is_integer(RoleId)->
    case role_black:is_black(RoleId) of
        ok ->
            case db:get_row("select role_id, phone from role where openid = ?", [OpenId]) of
                {ok, undefined} ->   %% openid没有绑定过
                    db:exec("update role set openid = ?  where role_id = ?", [OpenId, RoleId]),
                    case role_data:get_online_role(RoleId) of
                        {ok, #online_role{pid = Pid, screat = Screat}} ->    %% 在线断线重连
                            case catch  gen_server:call(Pid, {reconnect, self(), IP, OpenId, RedId, PayId, Icon, NickName, Ss}) of
                                LoginRole = #p_role_info{} ->
                                    {ok, LoginRole, Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                {false, Reason} -> 
                                    {false, Reason};
                                _ ->
                                    ets:delete(online_role, RoleId),
                                    {false, ?error_busy}
                            end;
                        _ ->  
                            Screat = sys_rand:rand(10000, 999999),
                            case role_data:get_role_from_dets(RoleId) of
                                {ok, Term} ->
                                    case role_var:update_var(Term) of
                                        {ok, Role = #role{open_id = ""}} ->
                                            NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, sex = Sex, icon = Icon, name = NickName, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                            {ok, Pid} = role:start(NewRole),
                                            {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                        {ok, _} ->
                                            {false, ?error_phone_wx};
                                        _Err ->
                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                            {false, ?error_role_data}
                                    end;
                                [] ->
                                    case db:get_one("select info from role where role_id = ?", [RoleId]) of
                                        {ok, Info} ->
                                            case util:string_to_term(Info) of
                                                {ok, Term} ->
                                                    case role_var:update_var(Term) of
                                                        {ok, Role = #role{open_id = ""}} ->
                                                            NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, sex = Sex, icon = Icon, name = NickName, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                                            {ok, Pid} = role:start(NewRole),
                                                            {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                                        {ok, _} ->
                                                            {false, ?error_phone_wx};
                                                        _Err ->
                                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                            {false, ?error_role_data}
                                                    end;
                                                {error, _Err}->
                                                    ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId, _Err]),
                                                    {false, ?error_role_data}
                                            end;
                                        _Err ->
                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                            {false, ?error_busy}
                                    end;
                                _Err ->
                                    {false, ?error_busy}
                            end
                    end;
                {ok, [RoleId1, <<>>]} when RoleId1 =/= RoleId -> %% 微信号已经存在，没有绑定过手机
                    case Flag of
                        0 ->
                            {false, ?error_fix_wx_not_phone};
                        1 ->  %% 强制绑定手机，删除旧的微信号
                            do_delete_wx(RoleId1),
                            db:exec("update role set openid = ?  where role_id = ?", [OpenId, RoleId]),
                            case role_data:get_online_role(RoleId) of
                                {ok, #online_role{pid = Pid, screat = Screat}} ->    %% 在线断线重连
                                    case catch  gen_server:call(Pid, {reconnect, self(), IP, OpenId, RedId, PayId, Icon, NickName, Ss}) of
                                        LoginRole = #p_role_info{} ->
                                            {ok, LoginRole, Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                        {false, Reason} -> 
                                            {false, Reason};
                                        _ ->
                                            ets:delete(online_role, RoleId),
                                            {false, ?error_busy}
                                    end;
                                _ ->  
                                    Screat = sys_rand:rand(10000, 999999),
                                    case role_data:get_role_from_dets(RoleId) of
                                        {ok, Term} ->
                                            case role_var:update_var(Term) of
                                                {ok, Role = #role{open_id = ""}} ->
                                                    NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, sex = Sex, icon = Icon, name = NickName, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                                    {ok, Pid} = role:start(NewRole),
                                                    {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                                {ok, _} ->
                                                    {false, ?error_phone_wx};
                                                _Err ->
                                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                    {false, ?error_role_data}
                                            end;
                                        [] ->
                                            case db:get_one("select info from role where role_id = ?", [RoleId]) of
                                                {ok, Info} ->
                                                    case util:string_to_term(Info) of
                                                        {ok, Term} ->
                                                            case role_var:update_var(Term) of
                                                                {ok, Role = #role{open_id = ""}} ->
                                                                    NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, sex = Sex, icon = Icon, name = NickName, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                                                    {ok, Pid} = role:start(NewRole),
                                                                    {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                                                {ok, _} ->
                                                                    {false, ?error_phone_wx};
                                                                _Err ->
                                                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                                    {false, ?error_role_data}
                                                            end;
                                                        {error, _Err}->
                                                            ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId, _Err]),
                                                            {false, ?error_role_data}
                                                    end;
                                                _Err ->
                                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                    {false, ?error_busy}
                                            end;
                                        _Err ->
                                            {false, ?error_busy}
                                    end
                            end;
                        2 ->  %% 强制绑定微信，删除旧的手机号
                            case db:get_row("select phone, phone_screat from role where role_id = ?", [RoleId]) of
                                {ok, [<<>>, <<>>]} ->   %% 没有手机号
                                    {false, ?error_busy};
                                {ok, undefined} ->   %% 没有手机号
                                    {false, ?error_busy};
                                {ok, [Phone1, PhoneScreat1]} ->
                                    Phone = erlang:binary_to_list(Phone1),
                                    PhoneScreat = erlang:binary_to_list(PhoneScreat1),
                                    do_delete_phone(RoleId),
                                    db:exec("update role set phone = ?, phone_screat = ? where role_id = ?", [Phone, PhoneScreat, RoleId1]),
                                    case role_data:get_online_role(RoleId1) of
                                        {ok, #online_role{pid = Pid, screat = Screat}} ->    %% 在线断线重连
                                            case catch  gen_server:call(Pid, {reconnect, self(), IP, OpenId, RedId, PayId, Icon, NickName, Phone, PhoneScreat, Ss}) of
                                                LoginRole = #p_role_info{} ->
                                                    {ok, LoginRole, Screat, State#conn{pid_object = Pid, role_id = RoleId1}};
                                                {false, Reason} -> 
                                                    {false, Reason};
                                                _R ->
                                                    ets:delete(online_role, RoleId1),
                                                    {false, ?error_busy}
                                            end;
                                        _ ->  
                                            Screat = sys_rand:rand(10000, 999999),
                                            case role_data:get_role_from_dets(RoleId1) of
                                                {ok, Term} ->
                                                    case role_var:update_var(Term) of
                                                        {ok, Role} ->
                                                            NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, sex = Sex, phone = Phone, phone_screat = PhoneScreat, icon = Icon, name = NickName, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                                            {ok, Pid} = role:start(NewRole),
                                                            {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId1}};
                                                        _Err ->
                                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                            {false, ?error_role_data}
                                                    end;
                                                [] ->
                                                    case db:get_one("select info from role where role_id = ?", [RoleId1]) of
                                                        {ok, Info} ->
                                                            case util:string_to_term(Info) of
                                                                {ok, Term} ->
                                                                    case role_var:update_var(Term) of
                                                                        {ok, Role} ->
                                                                            NewRole = Role#role{socket_pid = self(), ip = IP, screat = Screat, sex = Sex, phone = Phone, phone_screat = PhoneScreat, icon = Icon, name = NickName, open_id = OpenId, red_openid = RedId, pay_openid = PayId, subscribe = Ss},
                                                                            {ok, Pid} = role:start(NewRole),
                                                                            {ok, role_conver:to_login_role(NewRole), Screat, State#conn{pid_object = Pid, role_id = RoleId1}};
                                                                        _Err ->
                                                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                                            {false, ?error_role_data}
                                                                    end;
                                                                {error, _Err}->
                                                                    ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId1, _Err]),
                                                                    {false, ?error_role_data}
                                                            end;
                                                        _Err ->
                                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                            {false, ?error_busy}
                                                    end;
                                                _Err ->
                                                    {false, ?error_busy}
                                            end
                                    end;
                                _ ->
                                    {false, ?error_busy}
                            end
                    end;
                _ ->
                    {false, ?error_fix_wx}
            end;
        {StartTime, EndTime} ->
            sys_conn:pack_send(self(), 1149, #m_1149_toc{start_time = StartTime, end_time = EndTime}),
            {false, ?error_black}
    end.


%% 创建一个新玩家
create({OpenId, NickName, Icon, Sex, ParentID, RedId, PayId, Channel, Phone, PhoneScreat}, State = #conn{ip = Ip}) ->
    Screat = sys_rand:rand(10000, 9999999),
    RoleId = auto_increment:get_auto_id(role),
    Now = date:unixtime(),
    Role = #role{
        role_id = RoleId
        ,socket_pid = self()
        ,regist_time = Now
        ,login_time = Now
        ,open_id = OpenId
        ,ip = Ip
        ,name = NickName
        ,icon = Icon
        ,sex = Sex
        ,screat = Screat
        ,parent_id = ParentID
        ,red_openid = RedId
        ,pay_openid = PayId
        ,channel = Channel
        ,phone = Phone
        ,phone_screat = PhoneScreat
    },
    case role_data:new_to_db(Role) of
        ok ->
            {ok, Pid} = role:start(Role),
            role_account_mgr:registe(Channel),
            friend_mgr:add_friend({RoleId, NickName, Icon}, ParentID),
            {ok, role_conver:to_login_role(Role, regist), Screat, State#conn{pid_object = Pid, account = RoleId}};
        _ ->
            {false, ?error_busy}
    end.
    
%% GM创建账号
create(RoleId, NickName, Icon, Phone, PhoneScreat) ->
    case db:get_one("select role_id from role where role_id = ?", [RoleId]) of
        {ok, undefined} ->
            Now = date:unixtime(),
            Role = #role{
                role_id = RoleId
                ,regist_time = Now
                ,login_time = Now
                ,name = NickName
                ,icon = Icon
                ,phone = Phone
                ,phone_screat = PhoneScreat
                ,gold = 20000
                ,coin = 1000000
                ,vip_charge = 0
                ,animal_flag = 99
            },
            ets:insert(white_role, #white_role{role_id = RoleId, name = Role#role.name, time = Now}),
            case role_data:new_to_db(Role) of
                ok ->
                    ok;
                _ ->
                    {false, ?error_busy}
            end;
        _ ->
            {false, id_exit}
    end.



do_delete_phone(RoleId) ->
    db:exec("update role set phone = ''  where role_id = ?", [RoleId]),
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid}} -> 
            Pid ! delete_phone;
        _ ->
            ok
    end.

do_delete_wx(RoleId) ->
    db:exec("update role set openid = '', red_openid = '', pay_openid = ''  where role_id = ?", [RoleId]),
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid}} -> 
            Pid ! delete_wx;
        _ ->
            ok
    end.


id_login(RoleId, State = #conn{ip = IP}) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid, screat = Screat}} ->    %% 在线断线重连
            case catch  gen_server:call(Pid, {reconnect, self(), IP}) of
                LoginRole = #p_role_info{} ->
                    {ok, LoginRole, Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                _ ->
                    ets:delete(online_role, RoleId),
                    {false, ?error_busy}
            end;
        _ ->  
            Screat = sys_rand:rand(10000, 999999),
            case role_data:get_role_from_dets(RoleId) of
                {ok, Term} ->
                    case role_var:update_var(Term) of
                        {ok, Role = #role{}} ->
                            {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, screat = Screat}),
                            {ok, role_conver:to_login_role(Role), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                        _Err ->
                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                            {false, ?error_role_data}
                    end;
                [] ->
                    case db:get_one("select info from role where role_id = ?", [RoleId]) of
                        {ok, Info} ->
                            case util:string_to_term(Info) of
                                {ok, Term} ->
                                    case role_var:update_var(Term) of
                                        {ok, Role = #role{}} ->
                                            {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, screat = Screat}),
                                            {ok, role_conver:to_login_role(Role), Screat, State#conn{pid_object = Pid, role_id = RoleId}};
                                        _Err ->
                                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                            {false, ?error_role_data}
                                    end;
                                {error, _Err}->
                                    ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId, _Err]),
                                    {false, ?error_role_data}
                            end;
                        _Err ->
                            ?ERR("数据库读取玩家数据错误~w", [_Err]),
                            {false, ?error_busy}
                    end;
                _Err ->
                    {false, ?error_busy}
            end
    end.

