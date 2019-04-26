%% @author weichengjun
%% @doc @todo Add description to game.
-module(main).

-include("common.hrl").
-include("role.hrl").

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([start/2
        ,stop/1
]).

-export([
         start/0
		 ,stop/0
         ,game_start/0
         ,game_stop/0
         ,gm_stop/0
         ,gm_start/0
         ,tick_role/0
     ]).
%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(APPS, [crypto, asn1, public_key, ssl, inets, emysql, main]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

start() ->	
   start_applications(?APPS).

start(_, _) ->
    case init:get_plain_arguments() of
        [Type] ->
            case main_sup:start_link(list_to_atom(Type)) of
                {ok, Pid} ->
                    {ok, Pid};
                Error ->
                    Error
            end;
        Err -> 
            ?INFO("======~p...", [Err]),
            {error, bag_args}
    end.
stop() ->
    game_stop(),
    tick_role(),                                %% 踢所有玩家下线
    ?INFO("节点[~s]正在关闭...", [node()]),
    try
        stop_applications(?APPS),
        ?INFO("节点[~s]已经关闭.", [node()])
    catch
        T:X ->
            ?ERR("节点[~s]关闭时发生异常[~w]: ~w", [node(), T, X]),
            {error, X}
    end.
   %% init:stop().

tick_role() ->
    case catch ets:tab2list(online_role) of
        List when is_list(List) ->
            [Pid ! stop||#online_role{pid = Pid} <-List],
            do_check();
        _ -> ok
    end.

do_check() ->
    timer:sleep(1000),
    case catch ets:tab2list(online_role)  of
        [] ->
            ok;
        _ ->
            ?INFO("玩家进程还没有全部结束，请等待....", []),
            do_check()
    end.
        



stop(_State) ->
    ok.

game_start() ->
    ets:insert(ets_env, {game_ready, true}).   %% 设置游戏标识，可以连接

game_stop() ->
    ets:insert(ets_env, {game_ready, false}).   %% 设置游戏标识，不可以连接

gm_start() ->
    ets:insert(ets_env, {gm, true}).   %% 设置gm

gm_stop() ->
    ets:insert(ets_env, {gm, false}).   %% 设置gm

%%    app_util:start([crypto, asn1, public_key, ssl, inets])

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc 按次序启动app
start_applications(Apps) ->
    manage_applications(
        fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        cannot_start_application,
        Apps
    ).

%% @doc 按启动时相反的次序关闭app
stop_applications(Apps) ->
    manage_applications(
        fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        cannot_stop_application,
        Apps
    ).

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    F = fun(App, Acc) ->
            case Do(App) of
                ok -> [App | Acc];
                {error, {SkipError, _}} -> Acc;
                {error, Reason} ->
                    lists:foreach(Undo, Acc),
                    throw({error, {ErrorTag, App, Reason}})
            end
    end,
    Iterate(F, [], Apps),
    ok.
