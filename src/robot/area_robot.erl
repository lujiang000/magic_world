%%----------------------------------------------------
%% 竞技场机器人
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(area_robot).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("role.hrl").
-include("common.hrl").
-include("animal.hrl").
-include("all_pb.hrl").

start_link(Id, Pid) ->
    gen_server:start_link(?MODULE, [Id, Pid], []).

init([Id, Pid]) ->
    Role = #role{role_id = Id, name = "", room_pid = Pid},
    put(num, 300),
    N = sys_rand:rand(1, 9),
    put(hit_rule, N),
    erlang:send_after(3000, self(), hit),
    put(animal_list, []),
    {ok, Role}.

handle_call(_Request, _From, Role) ->
    {noreply, Role}.

handle_cast(_Msg, Role) ->
    {noreply, Role}.


handle_info({tcp_send, Bin}, Role) ->
    case get(num) of
        0 ->
            ok;
        _ ->
            do_recieve(Bin)
    end,
    {noreply, Role};

handle_info(hit, Role = #role{room_pid = Pid, role_id = Id}) ->
    case get(num) of
        0 ->
            ok;
        Num ->
            AnimalList = get(animal_list),
            case AnimalList of
                [] -> 
                    erlang:send_after(1000, self(), hit);
                _ ->
                    N = get(hit_rule),
                    Time = do_hit_rule(N, Num),
                    [MaxX, MaxY] = animal_route:get_size(),
                    #p_animal{id = AnimalId, line_id = RouteId, point = Point} = sys_rand:rand_list(AnimalList),
                    {X, Y} = animal:get_xy(RouteId, Point),
                    case X >= 0 andalso X =< MaxX andalso Y >= 0 andalso Y =< MaxY of
                        true ->
                            Pid ! {hit, robot, Id, AnimalId},
                            put(num, Num - 1);
                        _ ->
                            ok
                    end,
                    erlang:send_after(Time, self(), hit)
            end
    end,
    {noreply, Role};


handle_info(add_point, Role) ->
    case get(num) of
        0 ->
            ok;
        _ ->
            List = get(animal_list),
            NewList = [A#p_animal{point = Point + 1} ||A = #p_animal{point = Point} <-List],
            put(animal_list, NewList),
            erlang:send_after(1000, self(), add_point)
    end,
    {noreply, Role};



handle_info(stop, Role) ->
    {stop, normal, Role};

handle_info(_Info, Role) ->
    {noreply, Role}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 击打规则
do_hit_rule(1, Num) ->
    if Num > 200 ->
            sys_rand:rand(100, 400);
        Num > 100 ->
            sys_rand:rand(400, 800);
        true ->
            sys_rand:rand(200, 300)
    end;
do_hit_rule(2, Num) ->
    if Num > 200 ->
            sys_rand:rand(200, 700);
        Num > 100 ->
            sys_rand:rand(400, 600);
        true ->
            sys_rand:rand(200, 500)
    end;
do_hit_rule(3, Num) ->
    if Num > 200 ->
            sys_rand:rand(300, 800);
        Num > 100 ->
            sys_rand:rand(200, 400);
        true ->
            sys_rand:rand(400, 600)
    end;
do_hit_rule(4, _Num) ->
    300;
do_hit_rule(5, _Num) ->
    sys_rand:rand(300, 800);
do_hit_rule(6, _Num) ->
    500;
do_hit_rule(7, _Num) ->
    400;
do_hit_rule(8, _Num) ->
    200;
do_hit_rule(9, _Num) ->
    sys_rand:rand(100, 700).



%% 收到数据
do_recieve(Bin) ->
    <<ErrorID:16, _DataSize:16, DateStatus:8, _Flag:32, Cmd:16, NewBin/binary>> = Bin,
    case packet:unpack(Cmd, DateStatus, NewBin, toc) of
        {ok, Cmd, Data} ->
            ok;
       %%     case Cmd of
       %%         1506 ->
       %%             #m_1506_toc{animals = List} = Data,
       %%             OldList = get(animal_list),
       %%             case get(game_start) of
       %%                 true ->
       %%                     ok;
       %%                 _ ->
       %%                     put(game_start, true),
       %%                     erlang:send_after(1000, self(), add_point)
       %%             end,
       %%             put(animal_list, List ++ OldList);
       %%         1509 ->
       %%             #m_1509_toc{ids = List} = Data,
       %%             OldList = get(animal_list),
       %%             List1 = [Id||#p_animal_die{id = Id} <-List],
       %%             NewList = [Animal||Animal = #p_animal{id = Id} <-OldList, not lists:member(Id, List1)],
       %%             put(animal_list, NewList);
       %%         1511 ->
       %%             #m_1511_toc{id = List} = Data,
       %%             OldList = get(animal_list),
       %%             NewList = [Animal||Animal = #p_animal{id = Id} <-OldList, not lists:member(Id, List)],
       %%             put(animal_list, NewList);
       %%         _ ->
       %%             ok
       %%     end;
        _ ->
            ?ERR("解包数据出错:~w", [Cmd])
    end.
