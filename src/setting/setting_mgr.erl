%%----------------------------------------------------
%% 游戏相关设置
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(setting_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,get/1
        ,set/2
        ,web_get/1
        ,web_set/2
        ,web_set/4
        ,add_white_role/1
        ,delete_white_role/1
        ,apply_add_white/1
        ,apply_delete_white/1
        ,get_white_role/0

        ,add_black_role/1
        ,delete_black_role/1
        ,apply_add_black/1
        ,apply_delete_black/1
        ,get_black_role/0

        ,add_charge/2
        ,apply_add_charge/2
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").
-include("role.hrl").

-record(setting, {
        type = 0           %% 类型
        ,value = 0         %% 值
    }
).


%% 目前的设置
%% animal_red_bag  红包是否开启
%% shop            商城相关设置

get(Type) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> {ok, Value};
        _ -> false
    end.

set(Type, Value) ->
    ets:insert(setting, #setting{type = Type, value = Value}).

%% 后台调用
web_get(Type = ?setting_animal) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, 0}]
    end;
web_get(Type = ?setting_animal_pre) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ ->  [{result, trunc(?animal_pre * 1000)}]

    end;
web_get(Type = ?setting_animal_white_pre) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ ->  [{result, trunc(?animal_white_pre * 1000)}]

    end;
web_get(Type = ?setting_animal_black_pre) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ ->  [{result, trunc(?animal_black_pre * 1000)}]

    end;
web_get(Type = ?setting_friend) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ ->  [{result, ?friend_setting}]

    end;

web_get(Type = ?setting_redbag) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, ?redbag_setting}]
    end;

web_get(Type = ?setting_lollipop_pre) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, trunc(?lollipop_pre * 1000)}]
    end;

web_get(Type = ?setting_kill_rank) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> []
    end;

web_get(Type = ?setting_kill_rank_on) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, 0}]
    end;

web_get(Type = ?setting_kill_rank_week_on) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, 0}]
    end;

web_get(Type = ?setting_new_year) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, []}]
    end;

web_get(Type = ?setting_coin_tree) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, []}]
    end;
web_get(Type = ?setting_gold_pick) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> [{result, []}]
    end;

web_get(Type = ?setting_kill_rank_week) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> []
    end;

web_get(Type = ?setting_girl) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> [{result, Value}];
        _ -> []
    end;

web_get(Type = ?setting_shop) ->
    case ets:lookup(setting, Type) of
        [#setting{value = {Start, End, Num}}] -> [{start_time, Start}, {end_time, End}, {num, Num}];
        _ -> []
    end.


web_set(Type, Value) ->
    ets:insert(setting, #setting{type = Type, value = Value}),
    true.

web_set(Type, Start, End, Num) when is_integer(Start) andalso is_integer(End) andalso is_integer(Num)->
    ets:insert(setting, #setting{type = Type, value = {Start, End, Num}}),
    shop:reload(),
    true.


%% 设置白名单
add_white_role(RoleId) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid, name = Name}} ->
            role:apply(async, Pid, {?MODULE, apply_add_white, []}),
            ets:insert(white_role, #white_role{role_id = RoleId, name = Name, time = date:unixtime()}),
            true;
        _ ->
            case role_data:get_role_from_dets(RoleId) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_add_white(Role1),
                    role_data:save(NewRole),
                    ets:insert(white_role, #white_role{role_id = RoleId, name = NewRole#role.name, time = date:unixtime()}),
                    true;
                _ ->
                    false
            end
    end.

apply_add_white(Role) ->
    {ok, Role#role{animal_flag = 99}}.

apply_delete_white(Role) ->
    {ok, Role#role{animal_flag = 0}}.

%% 剔除白名单
delete_white_role(RoleId) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, apply_delete_white, []}),
            ets:delete(white_role, RoleId),
            true;
        _ ->
            case role_data:get_role_from_dets(RoleId) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_delete_white(Role1),
                    role_data:save(NewRole),
                    ets:delete(white_role, RoleId),
                    true;
                _ ->
                    false
            end
    end.

%% 获取白名单
get_white_role() ->
    List = ets:tab2list(white_role),
    [[{role_id, RoleId}, {name, Name}, {time, Time}] ||#white_role{role_id = RoleId, name = Name, time = Time} <-List].

%% 设置黑名单
add_black_role(RoleId) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid, name = Name}} ->
            role:apply(async, Pid, {?MODULE, apply_add_black, []}),
            ets:insert(black_role, #white_role{role_id = RoleId, name = Name, time = date:unixtime()}),
            true;
        _ ->
            case role_data:get_role_from_dets(RoleId) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_add_black(Role1),
                    role_data:save(NewRole),
                    ets:insert(black_role, #white_role{role_id = RoleId, name = NewRole#role.name, time = date:unixtime()}),
                    true;
                _ ->
                    false
            end
    end.

apply_add_black(Role) ->
    {ok, Role#role{animal_flag = 97}}.

apply_delete_black(Role) ->
    {ok, Role#role{animal_flag = 0}}.

%% 剔除黑名单
delete_black_role(RoleId) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, apply_delete_black, []}),
            ets:delete(black_role, RoleId),
            true;
        _ ->
            case role_data:get_role_from_dets(RoleId) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_delete_black(Role1),
                    role_data:save(NewRole),
                    ets:delete(black_role, RoleId),
                    true;
                _ ->
                    false
            end
    end.

%% 获取黑名单
get_black_role() ->
    List = ets:tab2list(black_role),
    [[{role_id, RoleId}, {name, Name}, {time, Time}] ||#white_role{role_id = RoleId, name = Name, time = Time} <-List].



%% 增加vip
add_charge(RoleId, Charge) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, apply_add_charge, [Charge]});
        _ ->
            case role_data:get_role_from_dets(RoleId) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_add_charge(Role1, Charge),
                    role_data:save(NewRole);
                _ ->
                    false
            end
    end.

apply_add_charge(Role = #role{vip_charge = VipCharge}, Num) ->
    NewVip = vip:get_lev(VipCharge + Num),
    {ok, Role#role{vip = NewVip, vip_charge = VipCharge + Num}}.



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(setting, [set, named_table, public, {read_concurrency, true}, {keypos, #setting.type}]),
    dets:open_file(setting, [{file, "./dets/setting.dets"}, {keypos, #setting.type}, {type, set}]),
    ets:new(white_role, [set, named_table, public, {read_concurrency, true}, {keypos, #white_role.role_id}]),
    dets:open_file(white_role, [{file, "./dets/white_role.dets"}, {keypos, #white_role.role_id}, {type, set}]),
    ets:new(black_role, [set, named_table, public, {read_concurrency, true}, {keypos, #white_role.role_id}]),
    dets:open_file(black_role, [{file, "./dets/black_role.dets"}, {keypos, #white_role.role_id}, {type, set}]),
    ets:from_dets(setting, setting),
    ets:from_dets(white_role, white_role),
    ets:from_dets(black_role, black_role),
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
    ets:to_dets(setting, setting),
    util:close_dets(setting),
    dets:delete_all_objects(white_role),
    ets:to_dets(white_role, white_role),
    util:close_dets(white_role),
    dets:delete_all_objects(black_role),
    ets:to_dets(black_role, black_role),
    util:close_dets(black_role),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
