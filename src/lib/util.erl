%%----------------------------------------------------
%% 工具包
%% 
%% @author yeahoo2000@gmail.com
%% @end
%%----------------------------------------------------
-module(util).
-export(
    [
        sleep/1
        ,is_process_alive/1
        ,bool2int/1
        ,for/3
        ,md5/1
        ,floor/1
        ,check_range/3
        ,check_max/2
        ,check_min/2
        ,ceil/1
        ,readlines/1
        ,load/1
        ,save/2
        ,template/2
        ,replace/3
        ,rand/2
        ,rand_list/1
        ,rand_list/2
        ,rand_map/2
        ,keys_sort/2
        ,maps_sort/2
        ,tuplelist_to_record_string/1
        ,fbin/1
        ,fbin/2
        ,flist/1
        ,flist/2
        ,bjoin/2
        ,cn/1
        ,cn/2
        ,build_fun/1
        ,to_string/1
        ,to_list/1
        ,to_atom/1
        ,parse_qs/1
        ,parse_qs/3
        ,term_to_string/1
        ,string_to_term/1
        ,term_to_bitstring/1
        ,all_to_binary/1
        ,to_binary/1
        ,check_name/1
        ,text_filter/1
        ,text_filter/2
        ,text_banned/1
        ,text_banned/2
        ,check_text/2
        ,check_text/3
        ,time_left/2
        ,utf8_len/1
        ,sub_big_list/2
        ,one_to_list/2
        ,get_stacktrace/0
        ,seg_cross_rect/6
        ,seg_cross_circle/4
        ,close_dets/1
        ,remove_duplicate/1
        ,name_len_valid/3
        ,order_by_ascii_for_http_get/1
        ,format_get_params/1
        ,format_xml_params/1
        ,get_xml_info/2
        ,check_max_uint32/1
        ,to_16/1
        ,to_2/2
        ,save_float/2
        ,cancel_timer/1
        ,utf8_to_url/1
        ,sum_list/2
        ,in_circle/3
        ,to_ip_string/1
        ,check_xml/1
    ]
).
-include("common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(utf, [46,95]++lists:seq(48,57)++lists:seq(65,90)++lists:seq(97,122)).


%% 
sum_list(Key, List) ->
    sum_list(Key, List, 0).

sum_list(_Key, [], N) -> N;
sum_list(Key, [Tuple | L], N) ->
    sum_list(Key, L, element(Key, Tuple) + N).


%% @doc 检查进程是否存活(可检查远程节点上的进程)
%% <div>注意: 此函数的消耗比较高，非必要时不要使用</div>
-spec is_process_alive(pid()) -> true | false.
is_process_alive(P) when is_pid(P) ->
    case rpc:call(node(P), erlang, is_process_alive, [P]) of
        true -> true;
        false -> false;
        _ -> false
    end.

%% @doc 程序暂停执行时长(单位:毫秒)
-spec sleep(T::integer()) -> ok.
sleep(T) ->
    receive
    after
        T -> ok
    end.

%% @doc 将true,false原子转成对应的0,1整数
-spec bool2int(X::boolean()) -> 0 | 1.
bool2int(true) -> 1;
bool2int(false) -> 0.

%% @doc 模拟for循环
-spec for(Begin::integer(), End::integer(), Fun::function()) -> ok.
for(End, End, Fun) ->
    Fun(End),
    ok;
for(Begin, End, Fun) when Begin < End ->
    Fun(Begin),
    for(Begin + 1, End, Fun).

%% @doc 生成16位格式的md5值
-spec md5(iodata()) -> binary().
md5(Data) ->
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(Data))]).

%% @doc 取小于X的最大整数 
-spec floor(number()) -> integer().
floor(X) ->
    T = erlang:trunc(X),
    case X < T of
        true -> T - 1;
        _ -> T
    end.

%% @doc 取大于X的最小整数
-spec ceil(number()) -> integer().
ceil(X) ->
    T = erlang:trunc(X),
    case X > T of
        true -> T + 1;
        _ -> T
    end.

%% @doc 限制最大最小值
-spec check_range(Val, Min, Max) -> number() when
    Val :: number(),
    Min :: number(),
    Max :: number().
check_range(Val, Min, Max) ->
    if
        Val > Max -> Max;
        Val < Min -> Min;
        true -> Val
    end.

%% @doc 限制最大值
-spec check_max(Val::number(), Max::number()) -> number().
check_max(Val, Max) ->
    if
        Val > Max -> Max;
        true -> Val
    end.

%% @doc 限制最小值
-spec check_min(Val::number(), Min::number()) -> number().
check_min(Val, Min) ->
    if
        Val < Min -> Min;
        true -> Val
    end.


%% @doc 多键排行 
-spec keys_sort([pos_integer()], [tuple()]) -> [tuple()].
keys_sort([],TupleList) ->
    lists:reverse(TupleList);
keys_sort([{asc, H} | T], TupleList) -> %% 升序
    Fun = fun(E1, E2) -> element(H, E1) > element(H, E2) end,
    NewTupleList = lists:sort(Fun, TupleList),
    keys_sort(T,NewTupleList);
keys_sort([H|T], TupleList) ->
    NewTupleList = lists:keysort(H, TupleList),
    keys_sort(T,NewTupleList).

%% @doc 新的数据结构#{}排序处理
-spec maps_sort([any()], [#{}]) -> [#{}].
maps_sort([], List) ->
    lists:reverse(List);
maps_sort([{asc, H} | T], List) -> %% 升序
    Fun = fun(M1, M2) -> maps:get(H, M1) > maps:get(H, M2) end,
    NewList = lists:sort(Fun, List),
    maps_sort(T, NewList);
maps_sort([H | T], List) ->
    Fun = fun(M1, M2) -> maps:get(H, M1) =< maps:get(H, M2) end,
    NewList = lists:sort(Fun, List),
    maps_sort(T, NewList).

%% @doc 以行模式读取文件
-spec readlines(string()) -> {ok, list()} | {error, term()}.
readlines(FileName) ->
    case file:open(FileName, [read]) of
        {error, Reason} -> {error, Reason};
        {ok, F} -> get_all_lines(F, [])
    end.
get_all_lines(F, L) ->
    case io:get_line(F, "") of
        {error, Reason} -> {error, Reason};
        eof -> {ok, lists:reverse(L)};
        Line -> get_all_lines(F, [Line | L])
    end.

%% @doc 读取文件，并将内容转成term()
-spec load(string()) ->
    {ok, undefined} | {ok, term()} | {error, term()}.
load(File) ->
    case file:consult(File) of
        {error, Reason} -> {error, Reason};
        {ok, []} -> {ok, []};
        {ok, [Term]} -> {ok, Term};
        _ ->
            {error, load_file_is_list}
    end.

%% @doc 将一个term()写入文件
-spec save(string(), term()) -> ok | {error, term()}.
save(File, Term) ->
    case file:open(File, [write]) of
        {error, Reason} -> {error, Reason};
        {ok, F} ->
            io:format(F, "~p.", [Term]),
            file:close(F),
            ok
    end.

%% @doc 替换模板变量
-spec template(string(), [{atom(), string()}]) -> string() | {error, term()}.
template(File, Vars) ->
    case file:read_file(File) of
        {error, Reason} -> {error, Reason};
        {ok, Content} ->
            V = [{lists:concat(["{{", K, "}}"]), to_list(V)} || {K, V} <- Vars],
            template_replace(unicode:characters_to_list(Content), V)
    end.
template_replace(Text, []) -> Text;
template_replace(Text, [{K, V} | L]) ->
    %% 用re:replace性能差到不能看...
    %% T = re:replace(Text, K, V, [caseless, global]),
    T = replace(Text, K, V),
    template_replace(T, L).

%% @doc 替换字符串
%% @todo 有必要再优化下性能
-spec replace(string(), string(), string()) -> string().
replace([], _Search, _Replace) -> "";
replace(Str, Search, Replace) ->
    replace(Str, Search, Replace, length(Search), []).
replace(Str, Search, Replace, Len, Rtn) ->
    case string:str(Str, Search) of
        0 -> Rtn ++ Str;
        P ->
            S = string:substr(Str, 1, P - 1) ++ Replace,
            replace(string:substr(Str, P + Len), Search, Replace, Len, Rtn ++ S)
    end.

%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand(Min::integer(), Max::integer()) -> integer().
rand(Min, Max) when Max < Min ->
    ?ERR("随机数错误的区间:[~w:~w]:~w",[Min, Max, get_stacktrace()]),
    rand(Max, Min);
rand(Min, Min) -> Min;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    %% @todo 这个机制有必要改进下
    case get(rand_seed) of
        undefined ->
            %%Seed = case catch sys_rand:get_seed() of
            %%    N = {_, _, _} -> N;
            %%    _ -> erlang:timestamp()
            %%end,
            Seed = erlang:timestamp(),
            rand:seed(exs1024, Seed),
            put(rand_seed, Seed);
        _ ->
            ignore
    end,
    M = Min - 1,
    rand:uniform(abs(Max - M)) + M.

%% @doc 从一个list中随机取出一项
-spec rand_list(List::list()) -> undefined | term().
rand_list([]) -> undefined;
rand_list([I]) -> I;
rand_list(List) -> 
    Idx = rand(1, length(List)),
    get_term_from_list(List, Idx).
get_term_from_list([H | _T], 1) ->
    H;
get_term_from_list([_H | T], Idx) ->
    get_term_from_list(T, Idx - 1).

%% @doc 从一个list中按各项权重值随机取出一项 每项为tuple()
-spec rand_list(List::[tuple()], Pos::pos_integer()) -> undefined | tuple().
rand_list([], _Pos) -> undefined;
rand_list([I], _Pos) -> I;
rand_list(List, Pos) ->
    Sum = lists:sum([element(Pos, I) || I <- List]),
    RandVal = rand(1, Sum),
    get_rand_tuple(List, Pos, RandVal).
get_rand_tuple([H | T], Pos, RandVal) ->
    Rand = element(Pos, H),
    case RandVal =< Rand of
        true -> H;
        false -> get_rand_tuple(T, Pos, RandVal - Rand)
    end.

%% @doc 从一个list中按各项权重值随机取出一项 每项为新数据结构#{}
-spec rand_map(List::[tuple()], Pos::pos_integer()) -> undefined | tuple().
rand_map([], _Key) -> undefined;
rand_map([I], _Key) -> I;
rand_map(List, Key) ->
    Sum = lists:sum([maps:get(Key, Map) || Map <- List]),
    RandVal = rand(1, Sum),
    get_rand_map(List, Key, RandVal).
get_rand_map([H | T], Key, RandVal) ->
    Rand = maps:get(Key, H),
    case RandVal =< Rand of
        true -> H;
        false -> get_rand_map(T, Key, RandVal - Rand)
    end.

%% @doc 将tuplelist转成一个record字串(tuplelist中必须要有record_name这一项)
%% <div>注意: 此函数比较低效，在要求性能的情况下不能使用</div>
-spec tuplelist_to_record_string([{atom(), any()}]) -> string() | {error, tuplelist_to_record_not_found_record_name}.
tuplelist_to_record_string(L) ->
    case lists:keyfind(record_name, 1, L) of
        false -> {error, tuplelist_to_record_not_found_record_name};
        {_, RecName} ->
            Nl = lists:keydelete(record_name, 1, L),
            io_lib:format("#~ts{~ts}", [RecName, rec_items(Nl, [])])
    end.
rec_items([], L) -> string:join(lists:reverse(L), ", ");
rec_items([{K, V} | T], L) ->
    S = io_lib:format("~ts = ~p", [K, V]),
    rec_items(T, [S | L]).

%% @doc 返回格式化的二进制字符串
%% <ul>
%% <li>String: 待格式化的二进制字符串</li>
%% <li>Args: 格式化参数，跟{@link io_lib:format/3}相同</li>
%% </ul>
-spec fbin(bitstring() | string() | list()) -> binary().
fbin(Data) ->
    fbin(Data, []).
fbin(Data, Args) ->
    unicode:characters_to_binary(io_lib:format(Data, Args)).

%% @doc 组装list
flist(Data) ->
    flist(Data, []).
flist(Data, Args) ->
    io_lib:format(Data, Args).

%% @doc 把二进制字符串合并
-spec bjoin(BitstringList, Sep) -> bitstring() when
    BitstringList :: [bitstring()],
    Sep :: bitstring().
bjoin(BitstringList, Sep) ->
    bjoin(BitstringList, Sep, <<"">>).
bjoin([], _, Result) -> Result;
bjoin([String], _, <<"">>) -> String;
bjoin([String], Sep, Result) ->
    fbin(<<"~ts~ts~ts">>, [Result, Sep, String]);
bjoin([String|T], Sep, Result) ->
    Result1 = case Result of
        <<"">> -> String;
        _ -> fbin(<<"~ts~ts~ts">>, [Result, Sep, String])
    end,
    bjoin(T, Sep, Result1).

%% @doc 在控制台显示带中文的字符串
-spec cn(String) -> ok when
    String :: bitstring() | string().
cn(String) ->
    cn(String, []).

%% @doc 在控制台显示带中文的字符串
%% <ul>
%% <li>F: 待显示的中文字符串（可带格式化参数）</li>
%% <li>A: 格式化参数</li>
%% </ul>
-spec cn(F, A) -> ok when
    F :: string() | bitstring(),
    A :: [term()].
cn(F, A) when is_list(F) ->
    L = io_lib:format(F, A),
    io:format("~ts", [L]);
cn(F, A)  ->
    L = io_lib:format(F, A),
    io:format("~ts", [list_to_binary(L)]).

%% @doc 将任意类型的数据转成string()类型
-spec to_string(any()) -> string().
to_string(X) -> lists:flatten(io_lib:format("~w", [X])).

%% @doc 将任意类型的数据转成list()类型(主要用于控制台打印).
%% <div>注意:tuple类型有特殊处理</div>
-spec to_list(any()) -> list().
to_list(X) when is_integer(X)     -> integer_to_list(X);
to_list(X) when is_binary(X)      -> binary_to_list(X);
to_list(X) when is_float(X)       -> float_to_list(X);
to_list(X) when is_atom(X)        -> atom_to_list(X);
to_list(X) when is_binary(X)      -> binary_to_list(X);
to_list(X) when is_bitstring(X)   -> bitstring_to_list(X);
to_list(X) when is_pid(X)         -> pid_to_list(X);
to_list(X) when is_function(X)    -> erlang:fun_to_list(X);
to_list(X) when is_port(X)        -> erlang:port_to_list(X);
to_list(X) when is_tuple(X)       -> to_string(X);
to_list(X) when is_list(X)        -> X.

%% @doc 将任意类型的数据转成atom()类型
-spec to_atom(any()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) ->
    L = to_list(X),
    list_to_atom(L).

%% @doc 根据字符串内容生成函数
-spec build_fun(string()) -> function().
build_fun(String)->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, L} = erl_parse:parse_exprs(Tokens),
    B = erl_eval:new_bindings(),
    BS = erl_eval:bindings(B),
    {[F], []} = erl_eval:expr_list(L, BS),
    F.

%% @doc 解析 QueryString
-spec parse_qs(string()) -> list().
parse_qs(String) when is_bitstring(String) -> parse_qs(bitstring_to_list(String));
parse_qs(String) -> parse_qs(String, "&", "=").

%% @doc 按指定的字符切割字符串
-spec parse_qs(String, Token1, Token2) -> list() when
    String :: bitstring() | string(),
    Token1 :: list(),
    Token2 :: list().
parse_qs(String, Token1, Token2) when is_bitstring(String) ->
    parse_qs(bitstring_to_list(String), Token1, Token2);
parse_qs(String, Token1, Token2) ->
    [list_to_tuple(string:tokens(Kv, Token2)) || Kv <- string:tokens(String, Token1)].

%% @doc term序列化，term转换为string格式
-spec term_to_string(term()) -> string().
term_to_string(Term) -> io_lib:format("~w", [Term]).

%% @doc term序列化，term转换为bitstring
-spec term_to_bitstring(term()) -> bitstring().
term_to_bitstring(Term) -> list_to_bitstring(term_to_string(Term)).

%% @doc term反序列化，string转换为term
-spec string_to_term(String) -> {error, Reason} | {ok, term()} when
    String :: undefined | string() | bitstring(),
    Reason :: term().
string_to_term(undefined) -> {ok, undefined};
string_to_term("undefined") -> {ok, undefined};
string_to_term(String) when is_bitstring(String) ->
    string_to_term(binary_to_list(String));
string_to_term(String) ->
    S = re:replace(String, "<[0-9]+\\.[0-9]+\\.[0-9]+>", "undefined", [{return, list}, global]),
    case erl_scan:string(S ++ ".") of
        {ok, Tokens, _} -> erl_parse:parse_term(Tokens);
        {error, Err, _} -> {error, Err}
    end.

%% @doc 将列里的不同类型转行成字节型
%% @todo 貌似不够高效，可以优化下
%% <div>如 [&lt;&lt;"字节"&gt;&gt;, 123, asd, "assd"] 输出 &lt;&lt;"字节123asdassd"&gt;&gt;</div>
-spec all_to_binary(list()) -> binary().
all_to_binary(List) -> all_to_binary(List, []).
all_to_binary([], Result) -> list_to_binary(Result);
all_to_binary([P | T], Result) when is_list(P) ->
    all_to_binary(T, lists:append(Result, P));
all_to_binary([P | T], Result) when is_integer(P) ->
    all_to_binary(T, lists:append(Result, integer_to_list(P)));
all_to_binary([P | T], Result) when is_binary(P) ->
    all_to_binary(T, lists:append(Result, binary_to_list(P)));
all_to_binary([P | T], Result) when is_float(P) ->
    all_to_binary(T, lists:append(Result, float_to_list(P)));
all_to_binary([P | T], Result) when is_atom(P) ->
    all_to_binary(T, lists:append(Result, atom_to_list(P))).

%% @doc 将Val值转换为binary格式（8位二进制）
%% @todo 貌似没有什么用，考虑删除掉
-spec to_binary(integer()) -> binary().
to_binary(Val) when is_integer(Val) -> list_to_binary(integer_to_list(Val));
to_binary(Val) when is_float(Val) -> list_to_binary(float_to_list(Val));
to_binary(Val) when is_list(Val) -> list_to_binary(Val);
to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_atom(Val) -> to_binary(atom_to_list(Val));
to_binary(_Val) -> <<>>.

%% @doc 检查名称（会对长度、特殊字符屏蔽、敏感词屏蔽）
-spec check_name(bitstring()) -> ok | {false, Reason::bitstring()}.
check_name(Text) ->
    %% 检查长度
    case name_len_valid(Text, 2, 14) of
        false -> {false, "名称长度不合要求"};
        true ->
            %% 检查字符有效性
            case name_valid(Text) of
                false -> {false, "名称中含有系统限制字符"};
                true ->
                    %% 检查禁用词
                    case text_banned(Text) of
                        true -> {false, "建设和谐社会，请勿使用非法词汇"};
                        false -> ok
                    end
            end
    end.

%% @doc 检查文本是否含有英文半角符号等半角符号
-spec is_normal_text(Text::iodata()) -> boolean().
is_normal_text(Text) ->
    %% 反斜杠"\"的匹配，使用了双重转义，因为erlang里面"\"字符本身也需要转义
    case re:run(Text, "[\;\^\,\.\"\'\:\+\=\!\?\<\>\/\*\|\~\`\@\#\$\%\(\)\{\}\\\-\\\[\\\]\\\\]", [{capture, none}, caseless, unicode]) of
        match -> false;
        nomatch -> true
    end.

%% @doc 检查文本中是否含有禁用词
-spec text_banned(Text::iodata()) -> boolean().
text_banned(Text) ->
    case is_normal_text(Text) of
        false -> true;
        true ->
            case text_banned(Text, keywords:banned()) of
                true -> true;
                false -> text_banned(Text, keywords:fuck_hexie())
            end
    end.

%% @doc 检查文本中各项
-spec check_text(Text::iodata(), Len::non_neg_integer()) -> {false, bitstring()} | ok. 
check_text(Text, Len) ->
    case name_len_valid(Text, 0, Len) of
        false -> {false, "长度不合要求"};
        true ->
            %% 检查禁用词
            case text_banned(Text, keywords:banned()) of
                true -> {false, "请勿使用非法词汇"};
                false ->
                    case text_banned(Text, keywords:fuck_hexie()) of
                        true -> {false, "请勿使用非法词汇"};
                        false -> ok
                    end
            end
    end.

%% @doc 检查文本中各项
-spec check_text(Text::iodata(), Len::non_neg_integer(), Key::iodata()) -> {false, bitstring()} | ok. 
check_text(Text, Len, Key) ->
    case name_len_valid(Text, 0, Len) of
        false -> {false, flist("~ts长度不合要求", [Key])};
        true ->
            %% 检查禁用词
            case text_banned(Text, keywords:banned()) of
                true ->
                    {false, flist("~ts中请勿使用非法词汇", [Key])};
                false ->
                    case text_banned(Text, keywords:fuck_hexie()) of
                        true ->
                            {false, flist("~ts中请勿使用非法词汇", [Key])};
                        false ->
                            ok
                    end
            end
    end.

%% @doc 文字内容过滤，将关键词替换为"*"
%% <div>注意:使用的是默认关键启库,keywords.erl</div>
-spec text_filter(Text::iodata()) -> NewText::bitstring().
text_filter(Text) ->
    T1 = text_filter(Text, kwywords:banned()),
    T2 = text_filter(T1, kwywords:fuck_hexie()),
    list_to_bitstring(T2).

%% @doc 文字内容过滤，将关键词替换为"*"
-spec text_filter(Text::iodata(), Keywords::[string()]) -> string().
text_filter(Text, []) -> Text;
text_filter(Text, [H | L]) ->
    T = re:replace(Text, H, "\*", [caseless, global]),
    text_filter(T, L).

%% @doc 计算剩余时间，单位：毫秒
-spec time_left(TimeMax::integer(), Begin::erlang:timestamp()) -> integer().
time_left(TimeMax, Begin)->
    T = util:floor(TimeMax - timer:now_diff(erlang:timestamp(), Begin) / 1000),
    case T > 0 of
        true -> T;
        false -> 0
    end.

%% 字符串长度计算(支持中文)
-spec utf8_len(Str::bitstring()) -> error | non_neg_integer().
utf8_len(Str) ->
    case unicode:characters_to_list(Str) of
        CharList when is_list(CharList) -> string_width(CharList);
        _ -> error
    end.

%% 对大列表进行'--'操作
-spec sub_big_list(L1::list(), L2::list()) -> list().
sub_big_list(L1, L2) ->
    H1 = ordsets:from_list(L1),
    H2 = ordsets:from_list(L2),
    ordsets:subtract(H1, H2).

%% 把同样的对象克隆若干份，放进列表中
-spec one_to_list(Obj::term(), Num::non_neg_integer()) -> list().
one_to_list(Obj, Num) ->
    one_to_list(Obj, Num, []).
one_to_list(_, 0, Result) -> Result;
one_to_list(Obj, N, Result) -> one_to_list(Obj, N-1, [Obj|Result]).

%% @doc 获取调用栈信息
get_stacktrace() ->
    try 
        throw(a)
    catch
        _Type:_Err ->
            tl(erlang:get_stacktrace())
    end.

%% @doc 判断线段是否跟矩形相交
seg_cross_rect({Sx, Sy}, {Dx, Dy}, {X1, Y1}, {X2, Y1}, {X2, Y1}, {X2, Y2}) ->
    case point_in_rect({Sx, Sy}, {X1, Y1}, {X2, Y2})
        orelse point_in_rect({Dx, Dy}, {X1, Y1}, {X2, Y2}) of
        true -> true;
        false -> 
            case seg_in_rect({Sx, Sy}, {Dx, Dy}, {X1, Y1}, {X1, Y2})
                orelse seg_in_rect({Sx, Sy}, {Dx, Dy}, {X1, Y1}, {X2, Y1})
                orelse seg_in_rect({Sx, Sy}, {Dx, Dy}, {X2, Y2}, {X1, Y2})
                orelse seg_in_rect({Sx, Sy}, {Dx, Dy}, {X2, Y2}, {X2, Y1}) of
                true -> true;
                false -> false
            end
    end.

%% @doc 判断线段是否与圆相交
seg_cross_circle({Sx, Sy}, {Dx, Dy}, {Px, Py}, R) ->
    {Fx, Fy} = {Dx - Sx, Dy - Sy},
    {Ex, Ey} = {Px - Sx, Py - Sy},
    Cross1 = Fx * Ex + Fy * Ey,
    case Cross1 =< 0 of
        true -> R > math:sqrt(math:pow(Sx - Px, 2) + math:pow(Sy - Py, 2));
        false ->
            Cross2 = Fx * Fx + Fy * Fy,
            case Cross1 >= Cross2 of
                true -> R > math:sqrt(math:pow(Px - Dx, 2) + math:pow(Py - Dy, 2));
                false ->
                    NewCross = Cross1/Cross2,
                    NewPx = Sx + (Dx - Sx) * NewCross,
                    NewPy = Sy + (Dy - Sy) * NewCross,
                    R > math:sqrt(math:pow(Px - NewPx, 2) + math:pow(Py - NewPy, 2))
            end
    end.

%% 判断以X,Y,是否在X1,Y1为圆心R为半径的园内  true | false
in_circle({X, Y}, {X1, Y1}, R) ->
    X2 = X - X1,
    Y2 = Y - Y1,
    math:sqrt(math:pow(X2, 2) + math:pow(Y2, 2)) =< R.
    



%% @doc 关闭dets，有错则打印
close_dets(Name) ->
    case catch dets:close(Name) of
        ok -> ok;
        _Err -> ?ERR("关闭dets[~w]失败:~w", [Name, _Err])
    end.

%% @doc 列表去重
-spec remove_duplicate(L :: list()) -> list().
remove_duplicate([]) -> [];
remove_duplicate(L = [_|_]) -> sets:to_list(sets:from_list(L)).


%% @doc 限制uint32最大值 uint64 max=18446744073709551615
-spec check_max_uint32(Val::number()) -> number().
check_max_uint32(Val) ->
    if
        Val > 4294967295 -> 4294967295;
        true -> Val
    end.

%% 转16进制
to_16(Binary) when is_binary(Binary) ->
    list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(Binary)]);
to_16(Integer) when is_integer(Integer) ->
    list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- integer_to_list(Integer)]);
to_16(List) ->
    list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(unicode:characters_to_binary(List))]).

%% 16进制转2进制
to_2([], R) ->
    erlang:list_to_binary(lists:reverse(R));
to_2(Binary, R) when is_binary(Binary)->
    to_2(erlang:binary_to_list(Binary), R);
to_2(List, R) ->
    A = lists:sublist(List,2),
    NewList = List--A,
    NewR = [erlang:list_to_integer(A,16) | R],
    to_2(NewList, NewR).


%% @doc 按ASCII码从小到大排序 http get请求数据格式
order_by_ascii_for_http_get(List) ->
    L = [format_key(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- List],
    LL = lists:sort(L),
    string:join([H || H <- LL], "&").

%% @doc 打包http get请求数据格式
format_get_params(List) ->
    L = [format_key(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- List],
    string:join([H || H <- L], "&").

%% @doc 打包xml数据格式
format_xml_params(PropList) ->
    Body = string:join(["<" ++ atom_to_list(Key) ++ "><![CDATA[" ++ format_val(Val) ++ "]]></" ++ atom_to_list(Key) ++ ">"
        || {Key, Val} <- PropList], "\n"),
    "<xml>\n"++Body++"\n</xml>".

%% @doc xml格式解析
get_xml_info(Xml, Type) ->
    case catch check_xml(Xml) of
        {ok, XmlDocs} ->
            get_xml_val(XmlDocs, Type);
        Error ->
            Error
    end.

%% @doc 对一个浮点数Val1保留Val2位小数
save_float(Val1, Val2) when is_float(Val1)->
    erlang:list_to_float(erlang:float_to_list(Val1,[{decimals, Val2}]));
save_float(Val1, _) ->
    Val1.

%% @doc 关闭定时器
cancel_timer(undefined) -> ok;
cancel_timer(TimerRef) ->
    catch erlang:cancel_timer(TimerRef).


%% utf8 -> urlunicode
utf8_to_url(Data) when is_binary(Data)->
    Data2 = [T || <<T:8>> <= Data],
    utf8_to_url(Data2, []);
utf8_to_url(Data) when is_list(Data)->
    utf8_to_url(Data, []).

utf8_to_url([], Url) -> Url;
utf8_to_url([Utf8Code | Tail], Url) ->
    NewUrl =
        case lists:member(Utf8Code, ?utf) of
            true ->
                Url ++ [Utf8Code];
            _ ->
                Url ++ "%" ++ integer_to_list(Utf8Code, 16)
        end,
    utf8_to_url(Tail, NewUrl).


%% -----------------------------------------
%% 私有函数
%% -----------------------------------------

text_banned(_Text, []) -> false;
text_banned(Text, [H | T]) ->
    case re:run(Text, H, [{capture, none}, caseless, unicode]) of
        match -> true;
        _ -> text_banned(Text, T)
    end.

%% 检查名称长度规范
name_len_valid(undefined, _Min, _Max) -> false;
name_len_valid(Text, Min, Max) ->
    case catch unicode:characters_to_list(Text) of
        CharList when is_list(CharList) ->
            Len = string_width(CharList),
            Len =< Max andalso Len >= Min;
        _ -> false
    end.

%% 检查名字：只允许使用汉字(不含标点符号)、字母、数字和下划线
name_valid(Text) ->
    %% 貌似测试只有bitstring才能正确执行到结果
    %% unicode字符集：
    %% {FF00}-{FFEF} 通用ASCII全角标点符号集
    %% {3000}-{303F} 中日韩标点符号集
    %% {4E00}-{9FBF} 中日韩统一汉字
    case re:run(Text, "[^a-zA-Z0-9\\x{4E00}-\\x{9FA5}_\\x{306E}\\x{2116}\\x{706c}\\x{4e36}\\x{4e28}\\x{4e3f}\\x{2573}\\x{256c}\\x{5350}\\x{2103}\\x{0021}]", [{capture, none}, caseless, unicode]) of
        match -> false; %%<<"角色名只允许使用汉字、字母、数字和下划线">>}; %% 含有非法字符 
        nomatch -> true
    end.

%% 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
string_width(String) ->
    string_width(String, 0).
string_width([], Len) ->
    Len;
string_width([H | T], Len) ->
    case H > 255 of
        true ->
            string_width(T, Len + 2);
        false ->
            string_width(T, Len + 1)
    end.

%% 判断点是否在矩形内
point_in_rect({Sx, Sy}, {X1, Y1}, {X2, Y2}) ->
    max(X1, X2) >= Sx
    andalso min(X1, X2) =< Sx
    andalso max(Y1, Y2) >= Sy
    andalso min(Y1, Y2) =< Sy.

%% 判断相交辅助
cross_prod({Ax, Ay}, {Bx, By}, {Cx, Cy}) ->
    (Bx - Ax) * (Cy - Ay) - (By - Ay) * (Cx - Ax).

%% 判断线段是否相交
seg_in_rect({Ax, Ay}, {Bx, By}, {Cx, Cy}, {Dx, Dy}) ->
    max(Ax, Bx) >= min(Cx, Dx)
    andalso max(Cx, Dx) >= min(Ax, Bx)
    andalso max(Ay, By) >= min(Cy, Dy)
    andalso max(Cy, Dy) >= min(Ay, By)
    andalso (cross_prod({Cx, Cy}, {Dx, Dy}, {Ax, Ay}) * cross_prod({Cx, Cy}, {Bx, By}, {Dx, Dy}) >= 0)
    andalso (cross_prod({Ax, Ay}, {Bx, By}, {Cx, Cy}) * cross_prod({Ax, Ay}, {Dx, Dy}, {Bx, By}) >= 0).


check_xml(Xml) ->
    case xmerl_scan:string(Xml) of
        {XmlDocs, _Rest} ->
            {ok, XmlDocs};
        _ ->
            {error, 1}
    end.
get_xml_val(XmlDocs, Path) ->
    case catch xmerl_xpath:string(Path, XmlDocs) of
        [XmlElement] ->
            case XmlElement of
                #xmlElement{content = [Content]} ->
                    #xmlText{value = Value} = Content,
                    {ok, Value};
                #xmlElement{content = List} ->
                    List1 = [Value || #xmlText{value = Value}<-List],
                    {ok, lists:concat(List1)}
            end;
        _ ->
            print_error(XmlDocs)
    end.
print_error(XmlDocs) ->
    case catch xmerl_xpath:string("/xml/message", XmlDocs) of
        [XmlElement] ->
            #xmlElement{content = [Content]} = XmlElement,
            #xmlText{value = Value} = Content,
            case Value of
                [115,117,98,95,109,99,104,95,105,100,19982,
                    115,117,98,95,97,112,112,105,100,19981,
                    21305,37197] ->
                    {error, 63};
                _ ->
                    {error, 1}
            end;
        _ ->
            {error, 1}
    end.

format_key(Key) when is_atom(Key) ->
    atom_to_list(Key);
format_key(Key) -> Key.

format_val(Val) when is_list(Val) ->
    Val;
format_val(Val) when is_binary(Val) ->
    binary_to_list(Val);
format_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
format_val(Val) when is_float(Val) ->
    float_to_list(Val).


to_ip_string(Ip) when is_tuple(Ip) ->
    List = erlang:tuple_to_list(Ip),
    List1 = [to_string(A)|| A <-List],
    string:join(List1, ".").


