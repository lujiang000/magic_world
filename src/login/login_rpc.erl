%%----------------------------------------------------
%% @doc 登陆协议入口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(login_rpc).
-export([
        handle/3
    ]).

-include("all_pb.hrl").
-include("common.hrl").
-include("conn.hrl").
-include("error_msg.hrl").

%% 心跳包
handle(1098, _, State) ->
    {ok, #m_1098_toc{}, State};

%% 账号登陆过了
handle(_, _, #conn{pid_object = Pid}) when Pid =/= undefined-> 
    {false, ?error_repeat_login};

%% 微信登陆
handle(1001, #m_1001_tos{code = Code, parent_id = ParentId, red_code = RedCode, pay_code = PayCode, channel_id = Channel, role_id = RoleId, flag = Flag}, State) ->
    case login:weixin_login({Code, ParentId, RedCode, PayCode, Channel, RoleId, Flag}, State) of
        {ok, Info, Id, NewState} ->
            {ok, #m_1001_toc{info = Info, id = Id}, NewState};
        {false, Reason} ->
            {false, Reason}
    end;

%% 账号登陆
handle(1002, #m_1002_tos{account = Account}, State) ->
    case login:account_login(Account, State) of
        {ok, Info, Id, NewState} ->
            {ok, #m_1002_toc{info = Info, id = Id}, NewState};
        {false, Reason} ->
            {false, Reason}
    end;

    
%% 断线重连
handle(1003, #m_1003_tos{role_id = RoleId, id = Id}, State) ->
    case login:screat_login(RoleId, Id, State) of
        {ok, Info, Id, NewState} ->
            {ok, #m_1003_toc{info = Info, id = Id}, NewState};
        {false, Reason} ->
            {false, Reason}
    end;

%% 手机密码登陆
handle(1004, #m_1004_tos{phone = Phone, screat = Screat}, State) ->
    case is_list(Screat) andalso Screat =/= "" of
        true ->
            case login:phone_login({Phone, Screat}, State) of
                {ok, Info, Id, NewState} ->
                    {ok, #m_1004_toc{info = Info, id = Id}, NewState};
                {false, Reason} ->
                    {false, Reason}
            end;
        _ ->
            {false, ?error_screat_null}
    end;

%% 手机注册获取验证码
handle(1005, #m_1005_tos{phone = Phone}, State) ->
    case erlang:length(Phone) of
        11 ->
            Now = date:unixtime(),
            case get(phone_code) of
                {_, _, Time} when Time >= Now ->
                    {false, ?error_act};
                _ ->
                    Code = sys_rand:rand(1000, 9999),
                    put(phone_code, {Code, Phone, Now + 60}),
                    catch lib_juhe:send_msg(Phone, Code),
                    {ok, #m_1005_toc{}, State}
            end;
        _ ->
            {false, ?error_phone}
    end;

%% 手机验证码登陆
handle(1006, #m_1006_tos{phone = Phone, screat = Screat, id = Code, parent_id = ParentId, channel_id = Channel}, State) ->
    case is_list(Screat) andalso Screat =/= "" of
        true ->
            case get(phone_code) of
                {Code, Phone, _} ->
                    case login:phone_screat_login({Phone, Screat, ParentId, Channel}, State) of
                        {ok, Info, Id, NewState} ->
                            {ok, #m_1006_toc{info = Info, id = Id}, NewState};
                        {false, Reason} ->
                            {false, Reason}
                    end;
                _ ->
                    {false, ?error_phone_code}
            end;
        _ ->
            {false, ?error_screat_null}
    end;



handle(_Cmd, _Data, _) ->
    ?ERR("错误的协议:~w:data:~w", [_Cmd, _Data]),
    ok.
