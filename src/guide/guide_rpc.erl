%%----------------------------------------------------
%% @doc 新手指引协议
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(guide_rpc).
-export([handle/3]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").

%% 获取新手指引列表
handle(1801, _, _Role = #role{guide = Guide})->
    {reply, #m_1801_toc{steps = Guide}};

%% 完成一步新手指引
handle(1802, #m_1802_tos{step = Step}, Role = #role{guide = Guide})->
    {ok, #m_1802_toc{}, Role#role{guide = [Step | Guide]}};

%% 获取新手礼包领取情况
handle(1803, _, Role)->
    Data = guide:get_guide_gift(Role),
    {reply, Data};

%% 领取新手礼包 
handle(1804, _, Role)->
    case guide:reward_guide_gift(Role) of
        {ok, NewRole} ->
            {ok, #m_1804_toc{}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

handle(_Cmd, _Data, _)->
    ?ERR("错误的协议数据cmd:~w,data:~w", [_Cmd, _Data]),
    {false, 1}.
