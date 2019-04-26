%%----------------------------------------------------
%% 开发工具
%%
%% @author yeahoo2000@gmail.com
%% @end
%%----------------------------------------------------
-module(dev).
-export([
        get_all_erl/0
        ,file_list/2
        ,m/0
        ,m/1
        ,u/0
        ,u/1
        ,u/2
        ,edoc/0
        ,decompile/1
        ,pstack/1
        ,etop/0
        ,etop_mem/0
        ,etop_msg/0
        ,etop_cpu/0
        ,etop_stop/0
        ,gc_all/0
        ,fprof/3
        ,eprof_all/1
        ,eprof/2
        ,scheduler_usage/0
        ,scheduler_stat/0
        ,trace/1
        ,trace/2
        ,trace_stop/0
        ,proc_mem_all/1
        ,proc_mem/1
        ,statistics/3
        ,dump_log/0
        ,ets_rank/0
    ]
).


-include_lib("kernel/include/file.hrl").
-include("common.hrl").
%% 自定格式信息输出，相当于io:format，支持中文输出
-define(P(F, A),
    case os:type() of
        {win32, _} -> 
            %% io:format(F, A);
            io:format("~ts", [unicode:characters_to_binary(io_lib:format(F, A))]);
        _ -> io:format("~ts", [unicode:characters_to_binary(io_lib:format(F, A))])
    end).
-define(P(F), ?P(F, [])).


%% 检测方法的耗时
statistics(M, F, A) ->
    statistics(wall_clock),  
    erlang:apply(M, F, A),
    {_, Time} = statistics(wall_clock), 
    io:format("function need time:~w~n", [Time]).
%%-----------------------------------------------------------------
%% 一些查看和检测处理代码的工具
%%-----------------------------------------------------------------
%% 反编译
%% 确认线上运行代码是否正确，reltools没掌握好，升级偶尔出现问题
decompile(Mod) ->
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(code:which(Mod), [abstract_code]),
    io:format("~ts~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).

%% 进程栈
%% 类似于jstack，发现大量进程挂起，进程数过高，运行慢，hang住等问题用到
pstack(Reg) when is_atom(Reg) ->
    case whereis(Reg) of
        undefined -> undefined;
        Pid -> pstack(Pid)
    end;
pstack(Pid) ->
    io:format("~s~n", [element(2, process_info(Pid, backtrace))]).

%%进程CPU占用排名
etop() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 20}, {lines, 20}, {sort, reductions}]) end).

%%进程Mem占用排名
etop_mem() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 20}, {lines, 20}, {sort, memory}]) end).

etop_msg() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 20}, {lines, 20}, {sort, msg_q}]) end).

etop_cpu() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 2}, {lines, 20}, {sort, runtime}]) end).
%%停止etop
etop_stop() ->
    etop:stop().

%% 进程内存过高时，来一发，看看是内存泄露还是gc不过来
%% 对所有process做gc
gc_all() ->
    [erlang:garbage_collect(Pid) || Pid <- processes()].


%% 对MFA 执行分析，会严重减缓运行，建议只对小量业务执行
%% 结果:
%% fprof 结果比较详细，能够输出热点调用路径
fprof(M, F, A) ->
    fprof:start(),
    fprof:apply(M, F, A),
    fprof:profile(),
    fprof:analyse(),
    fprof:stop().

% 对整个节点内所有进程执行eprof, eprof 对线上业务有一定影响,慎用!
% 建议TimeoutSec<10s，且进程数< 1000，否则可能导致节点crash
% 结果:
% 输出每个方法实际执行时间（不会累计方法内其他mod调用执行时间）
% 只能得到mod - Fun 执行次数 执行耗时
eprof_all(TimeoutSec) ->
    eprof(processes() -- [whereis(eprof)], TimeoutSec).

eprof(Pids, TimeoutSec) ->
    eprof:start(),
    eprof:start_profiling(Pids),
    timer:sleep(TimeoutSec),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().

% 统计下1s每个调度器CPU的实际利用率(因为有spin wait、调度工作, 可能usage 比top显示低很多)
scheduler_usage() ->
    scheduler_usage(1000).
scheduler_usage(RunMs) ->
    erlang:system_flag(scheduler_wall_time, true),
    Ts0 = lists:sort(erlang:statistics(scheduler_wall_time)),
    timer:sleep(RunMs),
    Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)),
    erlang:system_flag(scheduler_wall_time, false),
    Cores = lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
                    {I, (A1 - A0) / (T1 - T0)} end, lists:zip(Ts0, Ts1)),
    {A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
                    {Ai + (A1 - A0), Ti + (T1 - T0)} end, {0, 0}, lists:zip(Ts0, Ts1)),
    Total = A/T,
    io:format("~w~n", [[{total, Total} | Cores]]).

% 统计下1s内调度进程数量(含义：第一个数字执行进程数量，第二个数字迁移进程数量)
scheduler_stat() ->
    scheduler_stat(1000).

scheduler_stat(RunMs) ->
    erlang:system_flag(scheduling_statistics, enable),
    Ts0 = erlang:system_info(total_scheduling_statistics),
    timer:sleep(RunMs),
    Ts1 = erlang:system_info(total_scheduling_statistics),
    erlang:system_flag(scheduling_statistics, disable),
    lists:map(fun({{Key, In0, Out0}, {Key, In1, Out1}}) ->
                {Key, In1 - In0, Out1 - Out0} end, lists:zip(Ts0, Ts1)).

%trace Mod 所有方法的调用
trace(Mod) ->
    dbg:tracer(),
    dbg:tpl(Mod, '_', []),
    dbg:p(all, c).

%trace Node上指定 Mod 所有方法的调用, 结果将输出到本地shell
trace(Node, Mod) ->
    dbg:tracer(),
    dbg:n(Node),
    dbg:tpl(Mod, '_', []),
    dbg:p(all, c).

%停止trace
trace_stop() ->
    dbg:stop_clear().

%% 内存高OOM 排查工具
%% etop 无法应对10w+ 进程节点, 下面代码就没问题了；找到可疑proc后通过pstack、message_queu_len 排查原因
proc_mem_all(SizeLimitKb) ->
    Procs = [{undefined, Pid} || Pid<- erlang:processes()],
    proc_mem(Procs, SizeLimitKb).

proc_mem(SizeLimitKb) ->
    Procs = [{Name, Pid} || {_, Name, Pid, _} <- release_handler_1:get_supervised_procs(),
        is_process_alive(Pid)],
    proc_mem(Procs, SizeLimitKb).

proc_mem(Procs, SizeLimitKb) ->
      SizeLimit = SizeLimitKb * 1024,
      {R, Total} = lists:foldl(fun({Name, Pid}, {Acc, TotalSize}) ->
          case erlang:process_info(Pid, total_heap_size) of
              {_, Size0} ->
                  Size = Size0*8,
                  case Size > SizeLimit of
                      true -> {[{Name, Pid, Size} | Acc], TotalSize+Size};
                      false -> {Acc, TotalSize}
                  end;
              _ -> {Acc, TotalSize}
              end
          end, {[], 0}, Procs),
      R1 = lists:keysort(3, R),
      {Total, lists:reverse(R1)}.



%% @doc 编译并热更新模块(使用标准调试模式)
-spec m() -> ok.
m() ->
    m(main),
    m(tester),
    m(data).
m(main) ->
    make(main, [{d, debug}, {d, disable_auth}, {d, enable_gm_cmd}]);
m(tester) ->
    make(tester, [{d, dbg_tester}, {d, disable_auth}, {d, enable_gm_cmd}]);
m(data) ->
    make(data, []);
m(battle) ->
    make(battle, []);
m(data_and_battle) ->
    make(data, []),
    make(battle, []).

%% @doc 编译并更新模块
%% 有效编译参数:
%% <ul>
%% <li>debug            开启debug模式，打开宏?DEBUG的输出</li>
%% <li>dbg_sql          开启数据库调试模式，打印所有的SQL查询信息</li>
%% <li>dbg_socket       开启socket调试模式，打印所有收发的socket数据</li>
%% <li>dbg_lag          开启网络延时模拟，波动延时为100~300</li>
%% <li>enable_gm_cmd    开启GM命令</li>
%% <li>disable_auth     关闭ticket验证</li>
%% </ul>
-spec make(atom() | string(), list()) -> ok.
make(main, Param) ->
    make(env:get(code_path), Param);
make(data, Param) ->
    make(env:get(code_path) ++ "/src/data", Param);
make(battle, Param) ->
    make(env:get(code_path) ++ "/src/battle", Param);
make(tester, Param) ->
    make(env:get(code_path) ++ "/tester", Param);
make(Path, Param) ->
    util:cn("### 正在编译(~s)，参数:~w~n", [Path, Param]),
    file:set_cwd(Path),
    case make:all(Param) of
        up_to_date -> do_up([], false);
        _ -> ignore
    end,
    file:set_cwd(env:get(root)).

%% @doc 热更新所有模块(非强制)
-spec u() -> ok.
u() ->
    do_up([], false).

%% @doc 热更新所有模块(强制更新)
%% <ul>
%% <li>force 强制更新所有模块</li>
%% <li>[atom()] 非强制更新指定模块</li>
%% </ul>
-spec u(Options) -> ok when
    Options :: force | [atom()].
u(force) ->
    do_up([], true);
u(ModList) when is_list(ModList) ->
    do_up(ModList, false).

%% @doc 热更新指定模块(强制更新)
-spec u([atom()], F::force) -> ok.
u(ModList, force) when is_list(ModList) ->
    do_up(ModList, true).

%% @doc 生成API文档，将源码文件全部放入ebin/同级的tmp/目录中
-spec edoc() -> ok.
edoc() ->
    %% edoc:application(main, "./", []),
    {ok, Cwd} = file:get_cwd(),
    Dir = Cwd ++ "/src",
    case file_list(Dir, ".erl") of
        {error, _Why} -> ignore;
        {ok, L} ->
            edoc:files(do_edoc(L, []), [{new, true}, {dir, Cwd ++ "/doc"}])
    end,
    ok.

%% @doc 获取所有*.erl文件
%% <div>注意：对于没有访问权限的文件将不在出现在此列表中</div>
-spec get_all_erl() -> FileList | {error, term()} when
    FileList :: [{FileName, FilePath}],
    FileName :: string(),
    FilePath :: string().
get_all_erl() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = Cwd ++ "src",
    case file:list_dir(Dir) of
        {ok, L} ->
            {ok, file_filter(L, Dir, ".erl", [])};
        _Other ->
            {error, _Other}
    end.

%% @doc 获取指定目录下指定类型的文件(包括子目录)
-spec file_list(Dir, Ext) -> {ok, FileList} | {error, Why} when
    Dir :: string(),
    Ext :: string(),
    FileList :: [{FilePath, FileName}],
    Why :: term(),
    FilePath :: string(),
    FileName :: string().
file_list(Dir, Ext) ->
    %% ?ERR_MSG("搜索目录[~s]下的所有\"~s\"文件", [Dir, Ext]),
    case file:list_dir(Dir) of
        {error, Reason} -> {error, Reason};
        {ok, L} -> {ok, file_filter(L, Dir, Ext, [])}
    end.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------

%% 执行更新
do_up(L, F) ->
    util_1:cn("--- 正在热更新节点: ~w ---~n", [node()]),
    Args = case {L, F} of
        {[], false}         -> [];
        {[], true}          -> [force];
        {[_H | _T], false}  -> [L];
        {[_H | _T], true}   -> [L, force]
    end,
    print_up(apply(sys_code, up, Args)).

%% 显示更新结果
print_up([]) -> ?P("~n");
print_up([{M, ok} | T]) ->
    util:cn("# 加载模块成功: ~w~n", [M]),
    print_up(T);
print_up([{M, {error, Reason}} | T]) ->
    util:cn("* 加载模块失败[~w]: ~w~n", [M, Reason]),
    print_up(T).

%% 处理edoc使用的文件列表，过滤掉没有必要生成文档的文件
do_edoc([], L) -> L;
do_edoc([{M, F} | T], L) ->
    case util:text_banned(M, ["proto_.*", ".*_data", "sup_.*", ".*_rpc", "mysql.*"]) of
        true -> do_edoc(T, L);
        false -> do_edoc(T, [F | L])
    end.

%% 文件过滤，查找指定目录下的所有文件(包括子目录)，返回指定扩展名的文件列表
file_filter([], _Dir, _Ext, List) -> List;
file_filter([H | T], Dir, Ext, List) ->
    F = Dir ++ "/" ++ H,
    NewList = case file:read_file_info(F) of
        {ok, I} ->
            if
                I#file_info.type =:= directory ->
                    case file:list_dir(F) of
                        {ok, L} ->
                            D = Dir ++ "/" ++ H,
                            List ++ file_filter(L, D, Ext, []);
                        _Err ->
                            io:format("error in list directory:~w~n", [_Err]),
                            List
                    end;
                I#file_info.type =:= regular ->
                    case filename:extension(F) =:= Ext of
                        true ->
                            List ++ [{filename:basename(filename:rootname(F)), F}];
                        false ->
                            List
                    end;
                true ->
                    List
            end;
        _Other ->
            io:format("error in read_file_info:~w ~w~n", [F, _Other]),
            List
    end,
    file_filter(T, Dir, Ext, NewList).

dump_log() ->
    crashdump_viewer:start().

%% 查看ETS大小
ets_rank() ->
    List = ets:all(),
    List2 = [{Ets, ets:info(Ets, size)} || Ets <- List],
    F = fun({_, A}, {_, B}) -> A > B end,
    List3 = lists:sort(F, List2),
    Num = min(20, length(List3)),
    {Res, _} = lists:split(Num, List3),
    Res.
    
