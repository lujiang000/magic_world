%%----------------------------------------------------
%% @doc 邮件系统入口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(mail_rpc).
-export([
        handle/3
    ]).

-include("common.hrl").
-include("all_pb.hrl").

%% 查看所有邮件
handle(1401, _, Role) ->
    Data = mail_mgr:get_receive(Role),
    {reply, Data};

%% 领取邮件物品
handle(1402, #m_1402_tos{id = Id}, Role) ->
     case mail_mgr:get_items(Role, Id) of
         {ok, Items, NewRole} ->
             {ok, #m_1402_toc{list = Items}, NewRole};
         {false, Reason} ->
             {false, Reason}
     end;

%% 阅读邮件 
handle(1403, #m_1403_tos{id = Id}, Role) ->
    case mail_mgr:read(Role, Id) of
        {ok, NewRole} ->
            {ok, #m_1403_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 删除邮件 
handle(1404, #m_1404_tos{id = Id}, Role) ->
    NewRole = mail_mgr:delete(Role, Id),
    {ok, #m_1404_toc{}, NewRole};

%% 一键领取邮件物品
handle(1405, _, Role) ->
    {ok, Items, NewRole} = mail_mgr:get_all_items(Role),
    {ok, #m_1405_toc{list = Items}, NewRole};

%% 一键删除邮件
handle(1406, _, Role) ->
    NewRole = mail_mgr:delete_all(Role),
    {ok, #m_1406_toc{}, NewRole};


%% 错误的CMD
handle(_Cmd, _DataIn, _Role) ->
    ?ERR("错误的协议~w:~w", [_Cmd, _DataIn]),
    {error, 1}.
