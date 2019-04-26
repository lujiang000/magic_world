%%----------------------------------------------------
%% 全服任务系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(task_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,get_task/1
        ,do_init_task/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        list = []
        ,time = 0
    }).

-include("common.hrl").
-include("role.hrl").



%% 接取每日任务
get_task(Role = #role{daily_task = {_, Time}}) ->
    case gen_server:call(?MODULE, get_task) of
        {ok, List, Time1} ->
            case date:is_same_day(Time, Time1) of
                true -> Role;
                _ ->
                    Role#role{daily_task = {List, Time1}}
            end;
        _ -> Role
    end;
get_task(Role = #role{daily_task = {_, _, _}}) ->
    get_task(Role#role{daily_task = {[], 0}}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    TaskList = do_init_task(),
    ?INFO("[~w] 启动完成", [?MODULE]),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), flush),
    State = #state{list = TaskList, time = date:unixtime()},
    {ok, State}.

handle_call(get_task, _From, State = #state{list = List, time = Time}) ->
    {reply, {ok, List, Time}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, State) ->
    List = init_task(1),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), flush),
    {noreply, State#state{list = List, time = date:unixtime()}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 初始化任务
do_init_task() ->
    case db:get_all("select * from daily_task") of
        {ok, []} -> 
            init_task(0);
        {ok, [[_, String, Time]]} ->
            {ok, List} = util:string_to_term(String),
            Now = date:unixtime(),
            case date:is_same_day(Now, Time) of
                true ->
                    List;
                _ ->
                    init_task(1)
            end;
        _ ->
            []
    end.



init_task(Flag) ->
    List = init_task(Flag, 1, []),
    Time = date:unixtime(),
    case Flag of
        0 ->
            db:exec("insert into daily_task(id, list, time) values (?, ?, ?)", [1, util:term_to_string(List), Time]);
        _ ->
            db:exec("replace into daily_task(id, list, time) values (?, ?, ?)", [1, util:term_to_string(List), Time])
    end,
    List.


init_task(_Flag, 4, List) -> List;
init_task(Flag, Id, List) ->
    Mod = get_mod(Id),
    Id1 = sys_rand:rand_list(Mod:get_level(1)),
    Id2 = sys_rand:rand_list(Mod:get_level(2)),
    Id3 = sys_rand:rand_list(Mod:get_level(3)),
    Id4 = sys_rand:rand_list(Mod:get_level(4)),
    Id5 = sys_rand:rand_list(Mod:get_level(5)),
    Id6 = sys_rand:rand_list(Mod:get_level(6)),
    Id7 = sys_rand:rand_list(Mod:get_level(7)),
    Task1 = to_task(Mod, Id1),
    Task2 = to_task(Mod, Id2),
    Task3 = to_task(Mod, Id3),
    Task4 = to_task(Mod, Id4),
    Task5 = to_task(Mod, Id5),
    Task6 = to_task(Mod, Id6),
    Task7 = to_task(Mod, Id7),
    TaskList = [Task1, Task2, Task3, Task4, Task5, Task6, Task7],
    RoleTask = #role_daily_task{type = to_type(Id), list = TaskList},
    init_task(Flag, Id + 1, [RoleTask | List]).

%% 根据id获得配置模块
get_mod(1) -> mission_daily_1_setting; 
get_mod(2) -> mission_daily_2_setting; 
get_mod(3) -> mission_daily_3_setting.
%% 根据id获得房间类型
to_type(1) -> petty;
to_type(2) -> rich;
to_type(3) -> diamond.


to_task(Mod, Id) ->
    {_, {Type, Num}, Reward} = Mod:get_data(Id),
    #daily_task{id = Id, type = Type, target = Num, reward = Reward}.



