%%----------------------------------------------------
%% 数据库访问接口封装
%%
%% 使用的数据库驱动是emysql，这里的对一些常用的数据库
%% 操作进行重新封装，忽略了一些不常使用到的返回信息
%% 以方便使用，如果你需要这些返回信息，可以直接使用emysql
%% 的接口，目前这里提供的接口都不支持同时处理多条查询或更新，比如:
%% "select * from role where name = 'yeahoo'; update role set login = 1"
%%
%% @author yeahoo2000@gmail.com
%% @end
%%----------------------------------------------------
-module(db).
-export(
    [
        init/0
        ,exec/1
        ,exec/2
        ,select_limit/3
        ,select_limit/4
        ,get_one/1
        ,get_one/2
        ,get_row/1
        ,get_row/2
        ,get_all/1
        ,get_all/2
        ,format_sql/2
        ,insert_vals_sql/1
        ,select/2
        ,select/3
        ,select_or/3
        ,select_column/2
        ,insert/2
        ,insert_more/3
        ,update/3
        ,update_all/2
        ,replace/2
        ,delete/2
    ]
).
-include("common.hrl").
-include("emysql.hrl").

-define(NEW_SRV_LARGE_CONN_TIME, 86400).  %% 开新服后维持大连接的时间（单位：秒）

-define(DB, game).

%% @doc 数据库连接初始化
-spec init() -> ok.
init() ->
    {DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNumMin, DbConnNumMax}= sys_env:get_env(db_cfg),
    emysql:add_pool(?DB, DbConnNumMin, DbConnNumMax, DbUser, DbPass, DbHost, DbPort, DbName, DbEncode).

%% @doc 执行一条数据库语句，注意：不能用这个接口执行查询类的语句
-spec exec(bitstring() | string()) -> ok | {error, term()}.
exec(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #ok_packet{} -> ok;
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 执行一条数据库语句，带参数，注意：不能用这个接口执行查询类的语句
-spec exec(bitstring() | string(), [term()]) -> ok | {error, term()}.
exec(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #ok_packet{} -> ok;
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 执行分页查询，返回结果中的所有行
-spec select_limit(bitstring() | string(), non_neg_integer(), pos_integer()) ->
    {ok, [term()]} | {error, term()}.
select_limit(Sql, Offset, Num) ->
    S = list_to_binary([Sql, <<" limit ">>, integer_to_list(Offset), <<", ">>, integer_to_list(Num)]),
    case catch emysql:execute(?DB, S) of
        #result_packet{rows = Rows} -> {ok, Rows};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 执行分页查询(带格式化参数)，返回结果中的所有行
-spec select_limit(bitstring() | string(), [term()], non_neg_integer(), pos_integer()) ->
    {ok, [term()]} | {error, term()}.
select_limit(Sql, Args, Offset, Num) ->
    S = list_to_binary([Sql, " limit ", integer_to_list(Offset), ", ", integer_to_list(Num)]),
    case catch emysql:execute(?DB, S, Args) of
        #result_packet{rows = Rows} -> {ok, Rows};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行第一列(不带格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
-spec get_one(bitstring() | string()) -> {ok, term()} | {error, term()}.
get_one(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #result_packet{rows = []} -> {ok, undefined};
        #result_packet{rows = [[R]]} -> {ok, R};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行第一列(带有格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
%% -spec get_one(bitstring() | string(), [term()]) -> {ok, term()} | {error, term()}.
get_one(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #result_packet{rows = [[R]]} -> {ok, R};
        #result_packet{rows = []} -> {ok, undefined};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行
%% <div>注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{ok, undefined}</div>
-spec get_row(bitstring() | string()) -> {ok, [term()]} | {error, term()}.
get_row(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #result_packet{rows = [R]} -> {ok, R};
        #result_packet{rows = []} -> {ok, undefined};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行(带有格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{ok, undefined}</div>
-spec get_row(bitstring() | string(), [term()]) -> {ok, [term()]} | {error, term()}.
get_row(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #result_packet{rows = [R]} -> {ok, R};
        #result_packet{rows = []} -> {ok, undefined};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的所有行
-spec get_all(bitstring() | string()) -> {ok, term()} | {error, term()}.
get_all(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #result_packet{rows = R} -> {ok, R};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的所有行
-spec get_all(bitstring() | string(), [term()]) -> {ok, term()} | {error, term()}.
get_all(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #result_packet{rows = R} -> {ok, R};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 格式化SQL语句
-spec format_sql(Sql::bitstring(), Args::list()) -> bitstring().
format_sql(Sql, Args) ->
    NewArgs = [emysql_util:encode(A) || A <- Args],
    unicode:characters_to_binary(io_lib:format(Sql, NewArgs)).

%% @doc 创建插入语句值部分
-spec insert_vals_sql(Fields::list()) -> string().
insert_vals_sql(Fields) ->
    Sql = lists:concat(["(", string:join(lists:duplicate(length(Fields), "~ts"), ","), ")"]),
    binary_to_list(format_sql(Sql, Fields)).


select(Table, Where) ->
    {WhereKeys, WhereVals} = lists:unzip(Where),
    WhereStr = string:join(["`" ++ atom_to_list(Key) ++ "`=?" || Key <- WhereKeys], " AND "),
    Query = "SELECT * " ++ " FROM " ++ atom_to_list(Table) ++ " WHERE " ++ WhereStr,
    get_all(Query, WhereVals).

select(Column, Table, Where) ->
    ClmStr = string:join(["`" ++ atom_to_list(Key) ++ "`" || Key <- Column], ","),
    {WhereKeys, WhereVals} = lists:unzip(Where),
    WhereStr = string:join(["`" ++ atom_to_list(Key) ++ "`=?" || Key <- WhereKeys], " AND "),
    Query = "SELECT " ++ ClmStr ++ " FROM " ++ atom_to_list(Table) ++ " WHERE " ++ WhereStr,
    get_all(Query, WhereVals).

select_or(Column, Table, Where) ->
    ClmStr = string:join(["`" ++ atom_to_list(Key) ++ "`" || Key <- Column], ","),
    {WhereKeys, WhereVals} = lists:unzip(Where),
    WhereStr = string:join(["`" ++ atom_to_list(Key) ++ "`=?" || Key <- WhereKeys], " OR "),
    Query = "SELECT " ++ ClmStr ++ " FROM " ++ atom_to_list(Table) ++ " WHERE " ++ WhereStr,
    get_all(Query, WhereVals).

select_column(Column, Table) ->
    ClmStr = string:join(["`" ++ atom_to_list(Key) ++ "`" || Key <- Column], ","),
    Query = "SELECT " ++ ClmStr ++ " FROM " ++ atom_to_list(Table),
    get_all(Query).

%% insert
%% Table = atom()
%% Proplist = [{atom(), any()}]
insert(Table, Proplist) ->
    {Keys, Vals} = lists:unzip(Proplist),
    Holds = string:join(lists:duplicate(length(Proplist), "?"), ", "),
    KeysStr = string:join([atom_to_list(Key) || Key <- Keys], "`,`"),
    Query = "INSERT INTO `" ++ atom_to_list(Table) ++ "` (`" ++ KeysStr ++ "`) VALUES (" ++ Holds ++ ")",
    exec(Query, Vals).

%% insert
%% Table = atom()
%% Column = [atom()]
%% Proplist = [any()]
insert_more(Table, Keys, Vals) ->
    Holds = "("++string:join(lists:duplicate(length(Keys), "?"), ", ")++")",
    Count = length(Vals) div length(Keys),
    KeysStr = string:join([atom_to_list(Key) || Key <- Keys], "`,`"),
    HoldsStr = string:join(lists:duplicate(Count, Holds), ","),
    Query = "INSERT INTO `" ++ atom_to_list(Table) ++ "` (`" ++ KeysStr ++ "`) VALUES " ++ HoldsStr,
    exec(Query, Vals).

%% update
%% Table = atom()
%% Proplist = [{atom(), any()}]
%% Where = [{atom(), any()}]
update(Table, Proplist, Where) ->
    {Keys, Vals} = lists:unzip(Proplist),
    SetStr = string:join(["`" ++ atom_to_list(Key) ++ "`=?" || Key <- Keys], ","),
    {WhereKeys, WhereVals} = lists:unzip(Where),
    WhereStr = string:join(["`" ++ atom_to_list(Key) ++ "`=?" || Key <- WhereKeys], " AND "),
    Query = "UPDATE `" ++ atom_to_list(Table) ++ "` SET " ++ SetStr ++ " WHERE " ++ WhereStr,
    exec(Query, Vals ++ WhereVals).

update_all(Table, Proplist) ->
    {Keys, Vals} = lists:unzip(Proplist),
    SetStr = string:join(["`" ++ atom_to_list(Key) ++ "`=?" || Key <- Keys], ","),
    Query = "UPDATE `" ++ atom_to_list(Table) ++ "` SET " ++ SetStr,
    exec(Query, Vals).

%% replace
%% Table = atom()
%% Proplist = [{atom(), any()}]
replace(Table, Proplist) ->
    {Keys, Vals} = lists:unzip(Proplist),
    Holds = string:join(lists:duplicate(length(Proplist), "?"), ", "),
    KeysStr = string:join([ atom_to_list(Key) || Key <- Keys ], "`,`"),
    Query = "REPLACE INTO `" ++ atom_to_list(Table) ++ "` (`" ++ KeysStr ++ "`) VALUES (" ++ Holds ++ ")",
    exec(Query, Vals).


%% 删除一行
delete(Table, Where) ->
    {WhereKeys, WhereVals} = lists:unzip(Where),
    WhereStr = string:join(["`" ++ atom_to_list(Key) ++ "`=?" || Key <- WhereKeys], " AND "),
    Query = "DELETE FROM`" ++ atom_to_list(Table) ++ "`" ++ " WHERE " ++ WhereStr,
    exec(Query, WhereVals).



%% ----------------------------------------------------
%% 内部方法
%% ----------------------------------------------------
%% 根据开服时间获取数据库连接池最低连接数
%%get_min_conn_num(DbConnNumMin) ->
%%    Now = date:unixtime(),
%%    case env:get(open_time) of
%%        OpenTime when is_number(OpenTime) andalso Now - OpenTime < ?NEW_SRV_LARGE_CONN_TIME -> 500;
%%        _ -> DbConnNumMin
%%    end.

%% ----------------------------------------------------
%% 测试代码
%% ----------------------------------------------------


