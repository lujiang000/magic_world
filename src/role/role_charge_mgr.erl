%%----------------------------------------------------
%% 玩家充值缓冲进程，针对玩家不在线充值
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_charge_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,is_charge/1
        ,get_list/0
        ,add_charge/1
        ,delete_charge/1
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {list = []}).

-include("common.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 是否正在充值处理
is_charge(RoleID) ->
    case catch gen_server:call(?MODULE, get_list) of
        {ok, List} ->
            lists:member(RoleID, List);
        _ ->
            true
    end.

%% 增加到充值队列
add_charge(RoleID) ->
    ?MODULE ! {add, RoleID}.

%% 剔除充值队列
delete_charge(RoleID) ->
    ?MODULE ! {delete, RoleID}.

get_list() ->
    gen_server:call(?MODULE, get_list).


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.


handle_call(get_list, _From, State = #state{list = List}) ->
    {reply, {ok, List}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 加入充值队列
handle_info({add, RoleID}, State = #state{list = List}) ->
    {noreply, State#state{list = [RoleID | List]}};
%% 剔除充值队列
handle_info({delete, RoleID}, State = #state{list = List}) ->
    NewList = lists:delete(RoleID, List),
    {noreply, State#state{list = NewList}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
