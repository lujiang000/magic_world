%%----------------------------------------------------
%% TCP监听处理
%% 
%% @author weichengjun
%% @end
%%----------------------------------------------------
-module(sys_listener).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-define(TCP_LISTEN_OPTION, [
    binary
    ,{packet, 0}
    ,{active, false}
    ,{reuseaddr, true}
    ,{nodelay, false}
    ,{delay_send, true}
    ,{exit_on_close, false}
    ,{send_timeout, 10000}
    ,{send_timeout_close, false}

]).

-define(acceptor_num, 10).

%%----------------------------------------------------
%% 对外接口
%%----------------------------------------------------

%% @doc 开启连接监听服务
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 关闭连接监听服务
-spec stop() -> ok.
stop() ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    supervisor:terminate_child(env:get(sup_name), sup_acceptor),
    supervisor:terminate_child(env:get(sup_name), sys_listener),
    ?INFO("[~w] 已经关闭...", [?MODULE]),
    ok.

%%----------------------------------------------------
%% 内部处理
%%----------------------------------------------------

init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    process_flag(trap_exit, true),
    Port = sys_env:get_env(port),
    case gen_tcp:listen(Port, ?TCP_LISTEN_OPTION) of
        {ok, LSock} ->
            ?INFO("[~w] 成功监听到端口:~w", [?MODULE, Port]),
            start_acceptor(?acceptor_num, LSock),
            ?INFO("[~w] 启动完成", [?MODULE]),
            {ok, state};
        {error, Reason}->
            ?ERR("[~w] 无法监听到~w:~w", [?MODULE, Port, Reason]),
            {stop, listen_failure, state}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------
%% 私有函数
%%----------------------------------------------------

start_acceptor(0, _LSock)-> ok;
start_acceptor(N, LSock)->
    supervisor:start_child(sup_acceptor, [LSock]),
    start_acceptor(N - 1, LSock).
