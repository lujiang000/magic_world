%%----------------------------------------------------
%% TCP Acceptor
%% 
%% @author weichengjun
%% @end
%%----------------------------------------------------
-module(sys_acceptor).

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

%% flash跨域策略文件内容
-define(FL_POLICY_FILE,         <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).

%% 监控服务器握手消息
-define(CLIENT_MONITOR,     <<"monitor_client---------">>).
%% WEB端通讯消息
-define(WEB_CONN,           <<"web_conn---------------">>).
%% 测试器握手消息
-define(CLIENT_TESTER,      <<"tester_client----------">>).
%% 游戏客户端握手消息
-define(CLIENT_GAME,        <<"game_client------------">>).
%% flash策略文件请求
-define(CLIENT_FL_POLICY_REQ,   <<"<policy-file-request/>\0">>).

%% 连接握手超时在DEBUG模式为2分钟，正常是10秒
-ifdef(debug).
-define(TIMEOUT_HANDSHAKE, 120000).
-else.
-define(TIMEOUT_HANDSHAKE, 10000).
-endif.

%%----------------------------------------------------
%% 对外接口
%%----------------------------------------------------

%% @doc 启动acceptor
-spec start_link(port()) -> ignore | {ok, pid()} | {error, term()}.
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%----------------------------------------------------
%% 内部处理
%%----------------------------------------------------

init([LSock]) ->
    ?INFO("[~w] 已启动...", [?MODULE]),
	self() ! loop,
    {ok, {LSock}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-define(wite_ip, {121, 32, 152, 39}).

handle_info(loop, State = {LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            case sys_env:get_env(game_ready) of    %% 游戏已经准备好了
                true ->
                    inet:setopts(Socket, [{active, once}]),
                    gen_tcp:controlling_process(Socket, spawn(fun() -> accept(Socket) end));
                _Err -> 
                    {ok, {Ip, _Port}} = inet:peername(Socket),
                    case Ip =:= ?wite_ip of
                        true -> 
                            inet:setopts(Socket, [{active, once}]),
                            gen_tcp:controlling_process(Socket, spawn(fun() -> accept(Socket) end));
                        _ ->
                            gen_tcp:close(Socket)
                    end
            end;
        {error, closed} ->
            ?INFO("连接关闭{error, closed}"),
            ignore;
        {error, Reason} ->
            ?ERR("接受socket连接时发生了未预料的错误:~w", [Reason])
    end,
    self() ! loop, %% 继续等待下一个
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------
%% 私有函数
%%----------------------------------------------------

%% 接受一个连接
accept(Socket) ->
     receive 
        {tcp, Socket, HeaderData} ->
             HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
             HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList],
             case lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1) of
                 false ->
                     gen_tcp:close(Socket);
                 {_, SecWebSocketKey} ->
                     Sha1 = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
                     Base64 = base64:encode(Sha1),
                     Handshake = [
                         <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                         <<"Upgrade: websocket\r\n">>,
                         <<"Connection: Upgrade\r\n">>,
                         <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                         <<"\r\n">>
                     ],
                     gen_tcp:send(Socket, Handshake),
                     create_conn(Socket)
             end;
        _Else ->
            gen_tcp:close(Socket)
    end.


%% 创建连接进程
create_conn(Socket) ->
    try
        {ok, {Ip, Port}} = inet:peername(Socket),
        %% ok = inet:setopts(Socket, [{packet, Packet}]),
        {ok, Pid} = sys_conn:create(Socket, Ip, Port),
        gen_tcp:controlling_process(Socket, Pid)
        %% ?DEBUG("成功建立一个新连接(类型:~p)", [ClientType])
    catch
        T:X ->
            ?ERR("建立连接失败[~w : ~w]", [T, X]),
            gen_tcp:close(Socket)
    end.
