%%----------------------------------------------------
%% 黑名单
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_black).
-behaviour(gen_server).
-export([start_link/0
        ,add_black/1
        ,is_black/1
        ,get_black_info/1
        ,delete_black/1
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-define(black_time, 300).  %% 默认5分钟

-record(role_black, {
        role_id = 0
        ,start_time = 0
        ,end_time = 0
        ,num = 1
    }
).

%% 增加黑名单
add_black(RoleID) ->
    Now = date:unixtime(),
    {StartTime, EndTime} = case ets:lookup(role_black, RoleID) of
        [Role = #role_black{num = Num}] ->
            Add = get_black_time(Num),
            ets:insert(role_black, Role#role_black{start_time = Now, end_time = Now + Add, num = Num + 1}),
            {Now, Now + Add};
        _ ->
            ets:insert(role_black, #role_black{role_id = RoleID, start_time = Now, end_time = Now + ?black_time}),
            {Now, Now + ?black_time}
    end,
    case role_data:get_online_role(RoleID) of
        {ok, #online_role{socket_pid = Pid}} ->
            sys_conn:pack_send(Pid, 1149, #m_1149_toc{start_time = StartTime, end_time = EndTime}),
            Pid ! timeout;
        _ ->
            ok
    end.

%% 是否黑名单
is_black(RoleID) ->
    Now = date:unixtime(),
    case catch ets:lookup(role_black, RoleID) of
        [#role_black{start_time = StartTime, end_time = EndTime}] when Now < EndTime ->
            {StartTime, EndTime};
        _ -> 
            ok
    end.

%% 接触黑名单
delete_black(RoleID) ->
    ets:delete(role_black, RoleID).

%% 获取封号详细信息
get_black_info(RoleID) ->
    case ets:lookup(role_black, RoleID) of
        [#role_black{start_time = StartTime, end_time = EndTime, num = Num}] -> [{start_time, StartTime}, {end_time, EndTime}, {num, Num}];
        _ -> []
    end.


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(role_black, [set, named_table, public, {keypos, #role_black.role_id}]),
    dets:open_file(role_black, [{file, "./dets/role_black.dets"},  {keypos, #role_black.role_id}, {type, set}]),
    ets:from_dets(role_black, role_black),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    ets:to_dets(role_black, role_black),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% 根据被封次数一次增加限制登陆时间 ，第一次为5分钟
get_black_time(1) -> 1800;  %% 30分钟
get_black_time(2) -> 7200;  %% 2个小时
get_black_time(3) -> 86400; %% 一天
get_black_time(4) -> 604800; %% 一个星期
get_black_time(5) -> 2592000; %% 一个月
get_black_time(_) -> 31536000. %% 一年
