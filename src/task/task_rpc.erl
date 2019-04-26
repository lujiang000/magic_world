%%----------------------------------------------------
%% @doc 任务
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(task_rpc).
-export([handle/3]).



%% 获取任务信息
handle(1701, _, Role)->
    Data = task:get_task(Role),
    {reply, Data};

%% 获取新手任务信息
handle(1704, _, Role)->
    Data = task:get_guide_task(Role),
    {reply, Data};

%% 获取每日任务信息
handle(1707, _, Role)->
    Data = task:get_daily_task(Role),
    {reply, Data};


handle(_, _, _)->
    {false, 1}.
