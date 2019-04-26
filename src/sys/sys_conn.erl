%%----------------------------------------------------
%% 连接器 
%% 客户端(可以是游戏客户端或是GM工具客户端等)在系统中的一个影射,
%% 负责收发数据，用来控制角色进程或其它进程
%% @author weichengjun
%% @end
%%----------------------------------------------------
-module(sys_conn).
-behaviour(gen_server).
-export([
        create/3
        ,pack_send/2
        ,pack_send/3
        ,pack_send/4
        ,pack_send_error/2
        ,pack_send_error/3
        ,pack_send_error/4
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("conn.hrl").

-define(MAX_LEN, 524288). %% 限制最大的单条协议长度


%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------

%% @doc 创建一个连接器
create(Socket, Ip, Port) ->
    gen_server:start(?MODULE, [Socket, Ip, Port], []).


%% @doc 打包并发送消息
-spec pack_send(pid(), pos_integer(), tuple()) -> ok.
pack_send(Cmd, Data) ->
    case get(socket_pid) of
        Pid when is_pid(Pid) ->
            pack_send(Pid, Cmd, 0, Data);
        _ ->
            ?ERR("不在人物进程不能调用:~w", [Cmd])
    end.
pack_send(ConnPid, Cmd, Data) ->
    pack_send(ConnPid, Cmd, 0, Data).
pack_send(ConnPid, Cmd, Flag, Data) ->
    case catch packet:pack(Cmd, Data, Flag) of
        {ok, Bin} ->
            case get(send_buff) of
                undefined ->
                    ConnPid ! {tcp_send, Bin};
                List ->
                    put(send_buff, List ++ [Bin])
            end;
        Err ->
            ?ERR("打包数据出错[Cmd: ~w][Err: ~w][Data: ~w][stacktrace:~w]", [Cmd, Err, Data, util:get_stacktrace()])
    end.

%% 错误消息特殊处理
pack_send_error(Cmd, Data) ->
    case get(socket_pid) of
        Pid when is_pid(Pid) ->
            pack_send_error(Pid, Cmd, 0, Data);
        _ ->
            ?ERR("不在人物进程不能调用:~w", [Cmd])
    end.
pack_send_error(ConnPid, Cmd, Data) ->
    pack_send_error(ConnPid, Cmd, 0, Data).
pack_send_error(ConnPid, Cmd, Flag, Data) ->
    case catch packet:pack_error(Cmd, Data, Flag) of
        {ok, Bin} ->
            ConnPid ! {tcp_send, Bin};
        Err ->
            ?ERR("打包数据出错[Cmd: ~w][Err: ~w][Data: ~w][stacktrace:~w]", [Cmd, Err, Data, util:get_stacktrace()])
    end.

%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------

init([Socket, Ip, Port]) ->
    process_flag(trap_exit, true),
%%    self() ! server_time,
    self() ! read_next,
    erlang:send_after(60000, self(), client_check),
    erlang:send_after(300000, self(), account_check),
    erlang:send_after(180000, self(), loop),
    State = #conn{socket = Socket, ip = Ip, port = Port},
%%    ?DEBUG("建立连接成功：~w", [self()]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 客户端状态检查，如果客户端一定时间内不发送指令则认定为已断线
handle_info(client_check, State = #conn{role_id = _Account, recv_count = RecvCount, last_recv_count = LastRecvCount}) ->
    case RecvCount > LastRecvCount of
        true ->
            erlang:send_after(60000, self(), client_check),
            {noreply, State#conn{last_recv_count = RecvCount}};
        false ->
%%            ?ERR("[~w]的客户端长时间未发送指令，可能已经断线", [_Account]),
            {stop, normal, State}
    end;

handle_info(read_next, State) ->
    read_next(State);

%% 检查是否已经验证了身份
handle_info(account_check, State = #conn{role_id = 0, ip = Ip}) ->
%%    ?ERR("客户端[~w]连接后长时间未登录帐号，强制断开连接", [Ip]),
    {stop, normal, State};

%% 发送服务器基准时间
handle_info(server_time, State) ->
%%    pack_send(self(), 1044, {date:unixtime()}),
%%    erlang:send_after(180000, self(), server_time),
    {noreply, State};

%% 发送socket数据
handle_info({tcp_send, Bin}, State = #conn{role_id = Account, socket = Socket, send_count = SendCount, error_send = ErrSend, debug = _Debug}) ->
    NewBin = packet:packet(Bin),
    case catch erlang:port_command(Socket, NewBin, [nosuspend]) of
        true ->
            {noreply, State#conn{send_count = SendCount + 1}};
        false ->
            {noreply, State#conn{error_send = ErrSend + 1}};
        Else ->
            ?ERR("帐号[~w]发送socket数据失败:~w", [Account, Else]),
            {noreply, State#conn{error_send = ErrSend + 1}}
    end;

%% -------------------------------------------------------------------
%% 处理socket数据读取结果
%% -------------------------------------------------------------------

%% 客户端断开了连接
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State = #conn{role_id = Account}) ->
%%    ?ERR("在服务端接收数据时，帐号[~w]关闭了socket连接", [Account]),
    {stop, normal, State};

%% 收到包头数据，检查长度，如果太长的数据则不接受
handle_info({inet_async, _Socket, _Ref, {ok, <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7>>}}, State = #conn{role_id = Account, read_head = true}) when Len > ?MAX_LEN->
    ?ERR("帐号[~w]发送的socket数据过长: ~w", [Account, Len]),
    {stop, normal, State};
%% 收到包头数据
handle_info({inet_async, Socket, _Ref, {ok, _Bin = <<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7>>}}, State = #conn{read_head = true}) ->
    %% ?DEBUG("包头长度:~w", [Len]),
    case Opcode of
        0 ->
            {noreply, State#conn{read_head = ok}};
        _ ->
            case Len  of
                126 -> 
                    prim_inet:async_recv(Socket, 2, 60000),
                    {noreply, State#conn{read_head = ok, len = 2 * 8, open_code = Opcode}};
                127 -> 
                    prim_inet:async_recv(Socket, 8, 60000),
                    {noreply, State#conn{read_head = ok, len = 8 * 8, open_code = Opcode}};
                _ -> 
                    prim_inet:async_recv(Socket, Len + 4, 60000),
                    {noreply, State#conn{read_head = false, length = Len, open_code = Opcode}}
            end
    end;

%% 收包头数据
handle_info({inet_async, Socket, _Ref, {ok, <<DataLen:16>>}}, State = #conn{len = 16, read_head = ok}) ->
    prim_inet:async_recv(Socket, DataLen + 4, 60000),
    {noreply, State#conn{read_head = false, length = DataLen}};
handle_info({inet_async, Socket, _Ref, {ok, <<DataLen:64>>}}, State = #conn{len = 64, read_head = ok}) ->
    prim_inet:async_recv(Socket, DataLen + 4, 60000),
    {noreply, State#conn{read_head = false, length = DataLen}};

%% 收到正常数据
handle_info({inet_async, _Socket, _Ref, {ok, Bin = <<_Masking:4/binary, _Payload/binary>>}}, State = #conn{length = Len, open_code = Opcode, bad_req_count = Count, read_head = false, role_id = Account, debug = _Debug}) ->
    DataBin = packet:unpacket(Bin, Len),
    case Opcode of
        2 ->  
            case DataBin of
                <<DataSize:16, DataStatus:8, Flag:32, Cmd:16, Data/binary>> ->
                    case DataSize of
                        0 -> 
                            case router:handle(Cmd, null, Flag, State) of
                                ok -> 
                                    read_next(State);
                                {ok, NewState} ->
                                    read_next(NewState);
                                {ok, Reply, NewState} ->
                                    pack_send(self(), Cmd, Flag, Reply),
                                    read_next(NewState);
                                {false, Reason} ->
                                    pack_send_error(self(), Cmd, Flag, Reason),
                                    read_next(State);
                                Error ->
                                    ?ERR("~w***unpack error*** cmd:~w Error:~w~n", [Account, Cmd, Error]),
                                    read_next(State)
                            end;
                        _ -> 
                            case catch packet:unpack(Cmd, DataStatus, Data) of
                                {ok, Cmd, Data1} ->
                                    case router:handle(Cmd, Data1, Flag, State) of
                                        ok -> 
                                            read_next(State);
                                        {ok, NewState} ->
                                            read_next(NewState);
                                        {ok, Reply, NewState} ->
                                            pack_send(self(), Cmd, Flag, Reply),
                                            read_next(NewState);
                                        {false, Reason} ->
                                            pack_send_error(self(), Cmd, Flag, Reason),
                                            read_next(State);
                                        _Error ->
                                            ?ERR("~w***unpack error*** cmd:~w ", [Account, Cmd]),
                                            read_next(State)
                                    end;
                                _Error ->
                                    ?ERR("~w***unpack error*** cmd:~w:~w ", [Account, Cmd, _Error]),
                                    role_black:add_black(Account),
                                    %%read_next(State#conn{bad_req_count = Count + 1})
                                    {stop, normal, State}
                            end
                    end;
                _ ->
                    %%role_black:add_black(Account),
                    ?ERR("~w发送空包", [Account]),
                    {stop, normal, State}
            end;
        _ -> 
            {stop, normal, State}
    end;

%% 收到异常数据
handle_info({inet_async, _Socket, _Ref, {ok, _Bin}}, State = #conn{bad_req_count = BadReq, role_id = Account, ip = IP}) when BadReq > 10 ->
    %% 最多记录10个错误，不要全部记录，避免被攻击
    ?ERR("客户端[ACC:~w, IP:~w]发送了过多无效请求,断开连接", [Account, IP]),
    {stop, normal, State};
    %% {noreply, State#conn{bad_req_count = BadReq + 1}};
handle_info({inet_async, Socket, _Ref, {ok, Bin}}, State = #conn{role_id = Account, ip = Ip, socket = Socket, bad_req_count = BadReq}) ->
    ?ERR("客户端[Acc:~w IP:~w]发送了无效请求: ~w", [Account, Ip, Bin]),
    {noreply, State#conn{bad_req_count = BadReq + 1}};
%% 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, _Reason}}, State = #conn{role_id = Account}) ->
    case _Reason of
        timeout -> 
%%            ?ERR("帐号[~w]读取socket数据出错:~w", [Account, _Reason]),
            ok;
        _ -> 
            ?ERR("帐号[~w]读取socket数据出错:~w", [Account, _Reason])
    end,
    {stop, normal, State};

%% 处理socket数据发送结果
handle_info({inet_reply, _Socket, ok}, State) ->
    {noreply, State};
handle_info({inet_reply, _Socket, {error, closed}}, State = #conn{role_id = Account}) ->
%%    ?ERR("在服务端发送数据时，帐号[~w]关闭了socket连接", [Account]),
    {stop, normal, State};
handle_info({inet_reply, _Socket, {error, timeout}}, State = #conn{error_send = ErrSend}) ->
    {noreply, State#conn{error_send = ErrSend + 1}};
handle_info({inet_reply, _Socket, _Else}, State = #conn{role_id = _Account}) ->
%%    ?ERR("帐号[~w]发送socket数据时发生了未预料的错误: ~w", [_Account, _Else]),
    {stop, normal, State};

handle_info({save_role_pid, RolePID, RoleID}, State) ->
	link(RolePID),
    {noreply, State#conn{role_id = RoleID, pid_object = RolePID}};

%% 主动通知结束进程
handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({'EXIT', Pid, normal}, State = #conn{pid_object = Pid, role_id = _Account}) ->
%%    ?ERR("帐号[~w]控制的目标进程正常退出", [Account]),
    {stop, normal, State};

%% 处理关联进程异常退出
handle_info({'EXIT', Pid, _Why}, State = #conn{role_id = _Account, pid_object = ObjectPid}) when Pid =:= ObjectPid ->
%%    ?ERR("帐号[~w]控制的目标进程[~w]异常退出:~w", [Account, Pid, Why]),
    {stop, normal, State};

%% 内部循环
handle_info(loop, State = #conn{pid_object = _ObjectPid}) ->
    %% 约每隔180秒执行一次GC
    garbage_collect(),
    %% case util:rand(1, 30) of
    %%     10 when is_pid(_ObjectPid) -> self() ! {'EXIT', _ObjectPid, kill_self}; %% 不定时断线 测试重连机制
    %%     _ -> skip
    %% end,

    %% 10秒后进行下次循环
    erlang:send_after(180000, self(), loop),
    {noreply, State};

%% 信息发送接收监控开关
handle_info({debug, Debug}, State = #conn{account = _Account}) when Debug =:= true orelse Debug =:= false ->
    {noreply, State#conn{debug = Debug}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #conn{ip = _Ip, role_id = _Account, socket = Socket, object = _Object, pid_object = _ObjectPid}) ->
    %% catch _Object:disconnect(_ObjectPid), %% 通知被控制对象连接已断开
    catch gen_tcp:close(Socket),
%%    ?ERR("帐号[~w]的连接进程已退出:~w", [_Account, _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------

%% 通知连接器读取下一条指令
read_next(State = #conn{socket = Socket, recv_count = RecvCount, read_head = false}) ->
    prim_inet:async_recv(Socket, 2, 60000),
    {noreply, State#conn{recv_count = RecvCount + 1, read_head = true}};
read_next(State) ->
    %% 上一个数据包还未读取完成，忽略掉
    {noreply, State}.


