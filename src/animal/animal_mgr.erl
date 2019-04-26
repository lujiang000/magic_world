%%----------------------------------------------------
%%  动物园管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(animal_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,enter_room/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        id = 0
        ,list =  []
    }
).

-include("role.hrl").
-include("common.hrl").
-include("animal.hrl").
-include("error_msg.hrl").


%% 进入房间
enter_room(Role = #role{status = ?status_normal, use_coin = Coin, guide_task = #guide_task{id = TaskId}}, Type) ->
    Need = animal:get_min_coin(Type, TaskId),
    {Max, _, _} = zoo_room_power_setting:get_data(Coin),
    case Max >= Need of
        true ->
            case catch gen_server:call(?MODULE, {get_room_pid, Type, TaskId}) of
                {ok, Pid} ->
                    case catch gen_server:call(Pid, {enter, role_conver:to_animal_role(Role)}) of
                        {ok, Data} ->
                            {ok, Data, Role#role{room_type = Type, room_pid = Pid, status = ?status_zone}};
                        _Err ->
                            {false, ?error_busy}
                    end;
                _Err ->
                    {false, ?error_busy}
            end;
        _ ->
            {false, ?error_act}
    end;
enter_room(_, _) ->
    {false, ?error_act}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    erlang:process_flag(min_bin_vheap_size, 1024*1024),
    erlang:process_flag(min_heap_size, 1024*1024),
    erlang:process_flag(priority, high),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.


%% 玩家请求进入房间，单人房间特殊处理
handle_call({get_room_pid, Type = single, TaskId}, _From, State = #state{id = NextId}) ->
    case animal_guide:start_link({Type, TaskId}, NextId) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State#state{id = NextId + 1}};
        _ ->
            {reply, false, State}
    end;


handle_call({get_room_pid, Type, _}, _From, State = #state{id = NextId, list = List}) ->
    case lists:keyfind(Type, 1, List) of
        {Type, RoomList} ->
            case find_room(RoomList) of
                Room = #animal_room{id = Id, pid = Pid, num = Num} ->
                    NewList = lists:keydelete(Id, #animal_room.id, RoomList),
                    NewList1 = [Room#animal_room{num = Num + 1} | NewList],
                    {reply, {ok, Pid}, State#state{list = lists:keyreplace(Type, 1, List, {Type, NewList1})}};
                _ ->
                    case start_room(Type, NextId) of
                        Room = #animal_room{pid = Pid} ->
                            NewList = [Room | RoomList],
                            {reply, {ok, Pid}, State#state{id = NextId + 1, list = lists:keyreplace(Type, 1, List, {Type, NewList})}};
                        _ ->
                            {reply, false, State}
                    end
            end;
        _ ->
            case start_room(Type, NextId) of
                Room = #animal_room{pid = Pid} ->
                    {reply, {ok, Pid}, State#state{id = NextId + 1, list = [{Type, [Room]} | List]}};
                _ ->
                    {reply, false, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 房间人数减少通知
handle_info({delete_room_num, Id, Type}, State = #state{list = List}) ->
    case lists:keyfind(Type, 1, List) of
        {Type, RoomList} ->
            case lists:keyfind(Id, #animal_room.id, RoomList) of
                #animal_room{num = Num, pid = Pid} when Num =< 1->             %% 房间没有人了 房间进程结束
                    NewList = lists:keydelete(Id, #animal_room.id, RoomList),
                    NewList1 = lists:keyreplace(Type, 1, List, {Type, NewList}),
                    Pid ! stop,
                    {noreply, State#state{list = NewList1}};
                Room = #animal_room{num = Num} ->
                    NewList = lists:keyreplace(Id, #animal_room.id, RoomList, Room#animal_room{num = Num - 1}),
                    NewList1 = lists:keyreplace(Type, 1, List, {Type, NewList}),
                    {noreply, State#state{list = NewList1}};
                _ ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;

%% 房间异常关闭
handle_info({delete, Id, Type}, State = #state{list = List}) ->
    case lists:keyfind(Type, 1, List) of
        {Type, RoomList} ->
            case lists:keyfind(Id, #animal_room.id, RoomList) of
                #animal_room{}->            
                    NewList = lists:keydelete(Id, #animal_room.id, RoomList),
                    NewList1 = lists:keyreplace(Type, 1, List, {Type, NewList}),
                    {noreply, State#state{list = NewList1}};
                _ ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 找房间有空位置的房间
find_room([]) -> false; 
find_room([Room = #animal_room{num = Num}| _L]) when Num < ?animal_max_num->
    Room;
find_room([_Room | L]) ->
    find_room(L).


%% 新开一个房间
start_room(Type, Id) ->
    case catch animal:start_link(Type, Id) of
        {ok, Pid} ->
            #animal_room{id = Id, type = Type, num = 1, pid = Pid};
        _E ->
            ?ERR("======~w", [_E]),
            false
    end.
            


