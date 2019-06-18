%%----------------------------------------------------
%% 农场管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(farm_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,open_farm/2
        ,get_farm_info/1
        ,enter/3
        ,login/1
        ,zero_flush/1
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    }
).
-include("common.hrl").
-include("farm.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").


%% 登陆处理, 之前有牧场关闭着，开启
login(Role = #role{role_id = RoleId}) ->
    case ets:lookup(farm, RoleId) of
        [Farm = #farm{farm_pid = Pid}] -> 
            case erlang:is_process_alive(Pid) of
                true ->
                    Pid ! login,
                    Role#role{farm_pid = Pid};
                _ ->
                    case catch gen_server:call(?MODULE, {start_farm, Farm}) of
                        {ok, Pid1} ->
                            Pid1 ! login,
                            Role#role{farm_pid = Pid1};
                        _ ->
                            Role#role{farm_pid = undefined}
                    end
            end;
        _ ->
            Role#role{farm_pid = undefined}
    end.

%% 活动
-define(start_time, 1559750400).
-define(end_time, 1560096000).

%% 0点更新
zero_flush(_Role = #role{role_id = RoleId, vip = Vip}) ->
    Now = date:unixtime(),
    case Now >= ?start_time andalso Now =< ?end_time of
        true ->
            case ets:lookup(farm, RoleId) of
                [#farm{farm_pid = Pid}] -> 
                    Pid ! {send, Vip},
                    List = farm:get_vip_animal(Vip),
                    sys_conn:pack_send(2123, #m_2123_toc{type = 1, list = List});
                _ ->
                    ok 
            end;
        _ -> ok
    end.


%% 解锁房间类型
open_farm(Role = #role{role_id = RoleId, vip = Vip}, Type) ->
    case ets:lookup(farm, RoleId) of
        [#farm{farm_pid = Pid}] -> 
            Gold = case Type of
                1 -> 1000;
                2 -> 2000;
                3 -> 3000
            end,
            case role_lib:do_cost_gold(Role, Gold) of
                {ok, NewRole} ->
                    case farm:open_farm(Pid, Type) of
                        ok -> 
                            log_db:log(farm_open_log, insert, [RoleId, Type, Gold, date:unixtime()]),
                            {ok, NewRole};
                        _R -> _R
                    end;
                {false, Reason} ->
                    {false, Reason}
            end;
        _ ->
            Gold = case Vip >= 1 of
                true -> 0;
                _ ->
                    1000
            end,
            case role_lib:do_cost_gold(Role, Gold) of
                {ok, NewRole} ->
                    Farm = to_farm(Role),
                    case catch gen_server:call(?MODULE, {start_farm, Farm}) of
                        {ok, Pid} ->
                            Now = date:unixtime(),
                            case Now >= ?start_time andalso Now =< ?end_time of
                                true ->
                                    Pid ! {send, Vip},
                                    List = farm:get_vip_animal(Vip),
                                    sys_conn:pack_send(2123, #m_2123_toc{type = 1, list = List});
                                _ -> ok
                            end,
                            log_db:log(farm_open_log, insert, [RoleId, Type, Gold, Now]),
                            {ok, NewRole#role{farm_pid = Pid}};
                        _ ->
                            {false, ?error_busy}
                    end;
                {false, Reason} ->
                    {false, Reason}
            end
    end.


%% 获取其他人牧场信息
get_farm_info(RoleId) ->
    case ets:lookup(farm, RoleId) of
        [_Farm = #farm{name = Name, farm_list = List}] -> 
            {ok, Name, [Type || #farm_info{type = Type}<-List]};
        _ ->
            {false, ?error_farm_exist}
    end.


%% 进入牧场
enter(_Role = #role{role_id = RoleId}, RoleId, _Type) -> {false, ?error_act};
enter(_Role = #role{room_type = Type}, _RoleId, Type) -> {false, ?error_act};
enter(Role = #role{}, RoleId, Type) ->
    case ets:lookup(farm, RoleId) of
        [Farm = #farm{farm_pid = Pid}] -> 
            case erlang:is_process_alive(Pid) of
                true ->
                    farm:enter(Role, Pid, Type);
                _ ->
                    case catch gen_server:call(?MODULE, {start_farm, Farm}) of
                        {ok, Pid1} ->
                            farm:enter(Role, Pid1, Type);
                        _ ->
                            {false, ?error_busy}
                    end
            end;
        _ ->
            {false, ?error_act}
    end.




start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(farm, [set, named_table, public, {keypos, #farm.role_id}]),
    dets:open_file(farm, [{file, "./dets/farm.dets"},  {keypos, #farm.role_id}, {type, set}]),
    ets:from_dets(farm, farm),
    ets:new(farm_log, [set, named_table, public, {keypos, #farm_log.id}]),
    dets:open_file(farm_log, [{file, "./dets/farm_log.dets"},  {keypos, #farm_log.id}, {type, set}]),
    ets:from_dets(farm_log, farm_log),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.


%% 开启牧场
handle_call({start_farm, Farm}, _From, State) ->
    Reply = case catch farm:start_link(Farm) of
        {ok, Pid} ->
            {ok, Pid};
        _ ->
            {false, ?error_busy}
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:to_dets(farm, farm),
    ets:to_dets(farm_log, farm_log),
    ?INFO("[~w] 完成关闭", [?MODULE]),
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_init() ->
    Fun = fun(Farm = #farm{role_id = _Id})->
            ets:insert(farm, Farm),
            continue
    end,
    dets:traverse(farm, Fun).

%% 转换牧场结构
to_farm(#role{role_id = RoleId, name = Name, icon = Icon, vip = Vip}) ->
    #farm{role_id = RoleId, name = Name, icon = Icon, vip = Vip, time = date:unixtime(), farm_list = [#farm_info{}]}.

