%%----------------------------------------------------
%%  广播管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(boradcast_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,boradcast/1
        ,trumpet/2
        ,login/1
        ,logout/1
        ,reconnect/1
        ,barrage/1
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        role_list = []   %% 人物列表{RoleId, SocketPid}
        ,msg = ""        %% 当前最新所需要发送的消息
        ,sent_msg = ""   %% 已经发送的消息
    }).

-include("common.hrl").
-include("all_pb.hrl").
-include("role.hrl").

-define(boradcast_normal, 1).
-define(boradcast_trumpet, 2).
-define(boradcast_barrage, 3).

%% 普通广播
boradcast(Msg) ->
    ?MODULE ! {boradcast, Msg}.

%% 弹幕
barrage(Msg) ->
    ?MODULE ! {barrage, Msg}.

%% 喇叭
trumpet(Name, Msg) ->
    ?MODULE ! {trumpet, Name, Msg}.

%% 登陆
login(#role{role_id = RoleId, socket_pid = SocketPid}) ->
    ?MODULE ! {login, RoleId, SocketPid}.

%% 离线
logout(#role{role_id = RoleId}) ->
    ?MODULE ! {logout, RoleId}.

%% 重连
reconnect(#role{role_id = RoleId, socket_pid = SocketPid}) ->
    ?MODULE ! {reconnect, RoleId, SocketPid}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({login, RoleId, SocketPid}, State = #state{role_list = RoleList}) ->
    {noreply, State#state{role_list = [{RoleId, SocketPid} | RoleList]}};

handle_info({logout, RoleId}, State = #state{role_list = RoleList}) ->
    {noreply, State#state{role_list = lists:keydelete(RoleId, 1, RoleList)}};

handle_info({reconnect, RoleId, SocketPid}, State = #state{role_list = RoleList}) ->
    {noreply, State#state{role_list = lists:keyreplace(RoleId, 1, RoleList, {RoleId, SocketPid})}};

%% 普通广播
handle_info({boradcast, Msg1}, State = #state{msg = Msg}) ->
    case Msg of
        "" ->
            self() ! send_msg;
        _ ->
            ok
    end,
    {noreply, State#state{msg = Msg1}};

%% 推送普通广播
handle_info(send_msg, State = #state{msg = Msg, sent_msg = Msg}) ->
    {noreply, State#state{msg = "", sent_msg = ""}};
handle_info(send_msg, State = #state{role_list = RoleList, msg = Msg}) ->
    Data = #m_1106_toc{type = ?boradcast_normal, name = "", msg = Msg},
    erlang:spawn(fun() -> [sys_conn:pack_send(Pid, 1106, Data)||{_, Pid} <-RoleList] end),
    erlang:send_after(500, self(), send_msg),
    {noreply, State#state{sent_msg = Msg}};

%% 喇叭
handle_info({trumpet, Name, Msg}, State = #state{role_list = RoleList}) ->
    Data = #m_1106_toc{type = ?boradcast_trumpet, name = Name, msg = Msg},
    [sys_conn:pack_send(Pid, 1106, Data)||{_, Pid} <-RoleList],
    {noreply, State};

%% 弹幕
handle_info({barrage,  Msg}, State = #state{role_list = RoleList}) ->
    Data = #m_1106_toc{type = ?boradcast_barrage, name = "", msg = Msg},
    [sys_conn:pack_send(Pid, 1106, Data)||{_, Pid} <-RoleList],
    {noreply, State};


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



