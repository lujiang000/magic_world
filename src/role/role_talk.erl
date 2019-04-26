%%----------------------------------------------------
%% @doc 好友聊天
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_talk).
-export([
        to_talk_list/3
        ,talk_to_other/3
        ,apply_receive/2
        ,send_sys_msg/1
    ]
).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").


%% 转换成前端聊天信息列表
to_talk_list([], [], List) -> List;
to_talk_list([], LeftList, List) -> 
    Sql = lists:concat(["select role_id, name, icon, vip from role where role_id in ( ", string:join(lists:duplicate(erlang:length(LeftList), "?"), ","), " )"]),
    case catch db:get_all(Sql, LeftList) of
        {ok, List1} ->
            List2 = [#p_talk_role{role_id = RoleId, name = Name, icon = Icon, vip = Vip, status = 0}|| [RoleId, Name, Icon, Vip]<-List1],
            List ++ List2;
        _ ->
            List
    end;
to_talk_list([RoleID | L], LeftList, List) -> 
    case role_data:get_online_role(RoleID) of
        {ok, #online_role{name = Name, icon = Icon, vip = Vip}} ->
            TalkRole = #p_talk_role{role_id = RoleID, name = Name, icon = Icon, vip = Vip, status = 1},
            to_talk_list(L, LeftList, [TalkRole | List]);
        _ ->
            to_talk_list(L, [RoleID | LeftList], List)
    end.


%% 发送聊天
talk_to_other(Role = #role{role_id = RoleID, name = Name, icon = Icon, vip = Vip, talk_list = List}, ToId, Msg) ->
    case Vip >= 1 of
        true ->
            case role_data:get_online_role(ToId) of
                {ok, #online_role{pid = Pid}} ->
                    case role_lib:do_cost_gold(Role, 1) of
                        {ok, NewRole} ->
                            role:apply(async, Pid, {?MODULE, apply_receive, [#m_1147_toc{role_id = RoleID, name = Name, icon = Icon, vip = Vip, message = Msg}]}),
                            NewList = lists:delete(ToId, List),
                            {ok, NewRole#role{talk_list = lists:sublist([ToId | NewList], 10)}};
                        {false, Reason} ->
                            {false, Reason}
                    end;
                _ ->
                    {false, ?error_online_role}
            end;
        _ ->
            {false, ?error_vip}
    end.

%% 接收聊天
apply_receive(Role = #role{talk_list = List}, Data = #m_1147_toc{role_id = RoleID}) ->
    sys_conn:pack_send(1147, Data),
    NewList = lists:delete(RoleID, List),
    {ok, Role#role{talk_list = lists:sublist([RoleID | NewList], 10)}}.




%% 给所有在线玩家推送信息
send_sys_msg(Msg) ->
    Msg1 = case catch unicode:characters_to_list(erlang:list_to_binary(Msg)) of
        List1 when is_list(List1) -> List1;
        _ -> Msg
    end,
    List = ets:tab2list(online_role),
    [sys_conn:pack_send(Pid, 1148, #m_1148_toc{msg = Msg1})||#online_role{socket_pid = Pid} <-List],
    true.


