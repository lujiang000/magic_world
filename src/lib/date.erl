%%----------------------------------------------------
%% 日期函数，作为calendar模块的补充，calendar模块已有的功能不再提供
%% 
%% @author yeahoo2000@gmail.com
%% @end
%%----------------------------------------------------
-module(date).
-export(
    [
        unixtime/0
        ,unixtime/1
        ,is_valid/2
        ,is_same_day/2
        ,now_diff/2
        ,next_diff/3
        ,next_diff/2
        ,next_diff/1
        ,week_next_diff/2
        ,week_next_diff/3
        ,diff/2
        ,diff/3
        ,day_diff/2
        ,datetime_to_seconds/1
        ,seconds_to_datetime/1
        ,is_today/1
        ,is_same_week/2
        ,is_same_week/3
        ,is_same_month/2
        ,day_of_the_week/1
        ,day_of_the_week/3
        ,open_srv_day/0
        ,parse_time/1
        ,parse_time/2
        ,seconds_to_all_string/1
        ,seconds_to_string/1
        ,get_localtime_raw_string/0
    ]
).

-define(DAYS_PER_YEAR, 365).
-define(DAYS_PER_LEAP_YEAR, 366).

%% @doc 取得当前的unix时间戳
-spec unixtime() -> pos_integer().
unixtime() ->
    {M, S, _} = erlang:timestamp(),
    M * 1000000 + S.

%% @doc 返回相应类型的unix时间戳
%% <ul>
%% <li>ms: 取得当前的unix时间戳，精确到毫秒</li>
%% <li>zero: 获取当天0时0分0秒的时间戳</li>
%% <li>{zero, Ts}: 根据给出的时间戳，获取与该时间戳同一天的零时。当时间为0时，返回值有可能是负值，因为这里有时区偏移值(例如北京时间就可能是-28800)</li>
%% <li>{next_day, Ts}: 根据给出的时间戳，获取该时间戳第二天的零时</li>
%% <li>{next_time, Ts}::根据给出的天时间（距离0点的时间, 如3600），取出下一个该时间戳</li>
%% </ul>
-spec unixtime(X) -> pos_integer() when
    X :: ms | zero | {zero, pos_integer()} | {next_day, pos_integer()}.
unixtime(ms) ->
    {S1, S2, S3} = erlang:timestamp(),
    trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000);
unixtime(zero) ->
    {M, S, MS} = erlang:timestamp(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}),
    M * 1000000 + S - calendar:time_to_seconds(Time);
    %% unixtime() - calendar:time_to_seconds(time());
unixtime({zero, Ts}) ->
    Base = unixtime(zero),
    case Ts > Base of
        false -> Base - util:ceil((Base - Ts) / 86400) * 86400;
        true -> (Ts - Base) div 86400 * 86400 + Base
    end;
unixtime({next_day, Ts}) ->
    unixtime({zero, Ts}) + 86400;
unixtime({next_time, DayTs}) ->
    Now = unixtime(),
    NextT = next_diff(DayTs),
    Now + NextT.

%% @doc 计算开服第几天
-spec open_srv_day() -> non_neg_integer().
open_srv_day() ->
    OpenTime = env:srv_open_time(),  %% 开服时间
    Now = date:unixtime(),
    day_diff(OpenTime, Now) + 1. %% 开服第几天, 从1开始计数 

%% @doc 指定的时间戳与当前时间相比
%% @see diff/3
-spec diff(atom(), pos_integer()) -> pos_integer().
diff(T, Ts) ->
    diff(T, Ts, unixtime(ms)).

%% @doc 比较两个时间戳
%% <div>注意: 该函数只返回整数，比如3.5天会变成3天，也就是说比较的意思是相距3天或以上</div>
%% <ul>
%% <li>second: 比较两个时间相差的秒数<div>
%% <li>minute: 比较两个时间相差的分钟数<div>
%% <li>hour: 比较两个时间相差的小时数<div>
%% <li>day: 比较两个时间相差的天数</div>
%% </ul>
-spec diff(atom(), pos_integer(), pos_integer()) -> pos_integer().
diff(second, Ts1, Ts2) ->
    Ts1 - Ts2;
diff(minute, Ts1, Ts2) ->
    trunc((Ts1 - Ts2) / 60);
diff(hour, Ts1, Ts2) ->
    trunc((Ts1 - Ts2) / 3600);
diff(day, Ts1, Ts2) ->
    trunc((Ts1 - Ts2) / 86400).

%% @doc 两个unixtime相差的天数,相邻2天返回1
%% return int() 相差的天数
day_diff(FromTime, ToTime) when ToTime > FromTime ->
    FromDate = unixtime({zero, FromTime}),
    ToDate = unixtime({zero, ToTime}),
    case (ToDate - FromDate) / (3600 * 24) of
        Diff when Diff < 0 -> 0;
        Diff -> round(Diff)
    end;
day_diff(FromTime, ToTime) when ToTime =:= FromTime -> 0;
day_diff(FromTime, ToTime) -> day_diff(ToTime, FromTime).

%% @doc 取得当前距离指定时间下次到达时相差的秒数
-spec next_diff(H, M, S) -> Seconds when
    H :: 0..23,
    M :: 0..59,
    S :: 0..59,
    Seconds :: pos_integer().
next_diff(H, M, S) ->
    Sec = H * 3600 + M * 60 + S,
    next_diff(Sec).
-spec next_diff(0..86400 | [0..86400]) -> Seconds::pos_integer().
next_diff(L = [_ | _]) ->
    lists:min([next_diff(Sec) || Sec <- L]);
next_diff({H, M, S}) ->
    next_diff(H, M, S);
next_diff(Sec) ->
    %% Now = datetime(),
    %% next_diff(Sec, Now).
    DaySec = calendar:time_to_seconds(time()),
    case Sec > DaySec of 
        true -> Sec - DaySec;
        false -> Sec + 86400 - DaySec 
    end.
next_diff(Sec, T) ->
    Zero = unixtime({zero, T}),
    Base = Zero + Sec, %% 取当天距离X的时间为指定时间
    case Base > T of 
        true -> Base - T; %% 当前时间比指定时间小 直接返回差距
        false -> Base + 86400 - T %% 当前时间比指定时间大 加上一天时间后求差
    end.

%% @doc 取得当前距离指定时间下次到达时相差的秒数 指定周天
-spec week_next_diff([1..7], 0..86400 | [0..86400]) -> Seconds::pos_integer().
week_next_diff(WeekDays = [_ | _], 86400) ->
    week_next_diff(WeekDays, 86399) + 1;
week_next_diff(WeekDays = [_ | _], Sec) ->
    %% Now = unixtime(),
    %% week_next_diff(WeekDays, Now, Sec).
    DaySec = calendar:time_to_seconds(time()),
    case Sec > DaySec of 
        true -> do_week_next_diff(calendar:day_of_the_week(date()), WeekDays, Sec - DaySec, 0);
        false -> do_week_next_diff(calendar:day_of_the_week(date()) + 1, WeekDays, Sec + 86400 - DaySec, 0) 
    end.
week_next_diff(WeekDays = [_ | _], T, Sec) ->
    NextT = next_diff(T, Sec),
    WeekDay = day_of_the_week(T + NextT),
    do_week_next_diff(WeekDay, WeekDays, NextT, 0).
do_week_next_diff(_WeekDay, _WeekDays, _NextT, N) when N > 7 ->
    io:format("[~w:~w]error:WeekDays:~w", [?MODULE, ?LINE, _WeekDays]),
    0;
do_week_next_diff(WeekDay, WeekDays, NextT, N) ->
    case lists:member(WeekDay, WeekDays) of
        true -> NextT;
        false when WeekDay >= 7 -> do_week_next_diff(1, WeekDays, NextT + 86400, N + 1);
        false -> do_week_next_diff(WeekDay + 1, WeekDays, NextT + 86400, N + 1)
    end.

%% @doc 当前距离N天后某时刻的相差秒数
-spec now_diff(N, {H, I, S}) -> integer() when
    N :: today | tomorrow | non_neg_integer(),
    H :: 0..23,
    I :: 0..59,
    S :: 0..59.
now_diff(today, {H, I, S})  ->
    now_diff(0, {H, I, S});
now_diff(tomorrow, {H, I, S}) ->
    now_diff(1, {H, I, S});
now_diff(N, {H, I, S}) when is_integer(N), N >= 0 ->
    {_, Time} = calendar:local_time(),
    N * 86400 + calendar:time_to_seconds({H, I, S}) - calendar:time_to_seconds(Time).

%% @doc 检测是否为有效的日期或时间
-spec is_valid(T, Datetime) -> boolean() when
    T :: date | time | datetime,
    Datetime :: {Y, M, D} | {H, I, S} | {{Y, M, D}, {H, I, S}},
    Y :: pos_integer(),
    M :: pos_integer(),
    D :: pos_integer(),
    H :: non_neg_integer(),
    I :: non_neg_integer(),
    S :: non_neg_integer().
is_valid(date, Date) ->
    calendar:valid_date(Date);
is_valid(time, {H, I, S}) ->
    H >= 0 andalso H < 24 andalso I >= 0 andalso I < 60 andalso S >= 0 andalso S < 60;
is_valid(datetime, {Date, Time}) ->
    calendar:valid_date(Date) andalso is_valid(time, Time);
is_valid(_, _) ->
    false.

%% @doc 比较两个unixtime是否同一天
-spec is_same_day(Ts1, Ts2) -> boolean() when
    Ts1 :: pos_integer(),
    Ts2 :: pos_integer().
is_same_day(Ts1, Ts2) when Ts2 > Ts1 -> is_same_day(Ts2, Ts1);
is_same_day(Ts1, Ts2) when Ts1 - Ts2 > 86400 -> false;
is_same_day(Ts1, Ts2) ->
    unixtime({zero, Ts1}) =:= unixtime({zero, Ts2}).

%% @doc 判断一个时间戳是否为今天
-spec is_today(pos_integer()) -> boolean().
is_today(Unixtime) ->
    unixtime(zero) =:= unixtime({zero, Unixtime}).

%% @doc 判断两个unixtime是否为同一周
is_same_week(Ts1, Ts2) ->
    is_same_week(Ts1, Ts2, 0).
is_same_week(Ts1, Ts2, OffSet) when Ts1 > Ts2 ->
    is_same_week(Ts2, Ts1, OffSet);
is_same_week(Ts1, Ts2, _OffSet) when Ts2 - Ts1 >= 86400 * 7 ->
    false;
is_same_week(Ts1, Ts2, OffSet) ->
    %% {Date1, _} = seconds_to_datetime(Ts1),
    %% {Date2, _} = seconds_to_datetime(Ts2),
    %% calendar:iso_week_number(Date1) =:= calendar:iso_week_number(Date2).
    BaseT = 1388937600 + OffSet,   %% 2014年1月6日0点0分0秒 星期一
    WeekS = 604800,
    W1 = (Ts1 - BaseT) div WeekS,
    W2 = (Ts2 - BaseT) div WeekS,
    W1 =:= W2.

%% @doc 判断两个unixtime是否为同一月份
is_same_month(Ts1, Ts2) ->
    {{Y1, M1, _}, _} = date:seconds_to_datetime(Ts1),
    {{Y2, M2, _}, _} = date:seconds_to_datetime(Ts2),
    {Y1, M1} =:= {Y2, M2}. 

%% @doc 将日期转换unix时间戳
-spec datetime_to_seconds(DateTime) -> false | Seconds when
    DateTime :: {{Y, M, D}, {H, M, S}},
    Y :: pos_integer(),
    M :: 1..12,
    D :: 1..31,
    H :: 0..23,
    M :: 0..59,
    S :: 0..59,
    Seconds :: pos_integer().
datetime_to_seconds({Year, Month, Day, Hour, Minute, Second}) ->
    datetime_to_seconds({{Year, Month, Day}, {Hour, Minute, Second}});
datetime_to_seconds(DateTime) ->
    case get({datetime_to_seconds, DateTime}) of %% 增加缓存 时间计算很消耗CPU
        T when is_integer(T) -> T;
        _ ->
            case calendar:local_time_to_universal_time_dst(DateTime) of
                [] -> false;
                [_, Udate] -> 
                    T = calendar:datetime_to_gregorian_seconds(Udate) - 719528 * 24 * 3600,
                    put({datetime_to_seconds, DateTime}, T),
                    T;
                [Udate] ->
                    T = calendar:datetime_to_gregorian_seconds(Udate) - 719528 * 24 * 3600,
                    put({datetime_to_seconds, DateTime}, T),
                    T
            end 
    end.

%% @doc 将Unixtime转换成当地时间
-spec seconds_to_datetime(Unixtime) -> {{Y :: non_neg_integer(), M :: non_neg_integer(), D:: non_neg_integer()}, {HH :: non_neg_integer(), MM :: non_neg_integer, SS :: non_neg_integer()}} when
    Unixtime :: non_neg_integer().
seconds_to_datetime(Unixtime) ->
    LocalStamp = case get(local_stamp) of 
        T when is_integer(T) -> T;
        _ ->
            Local = erlang:universaltime_to_localtime({{1970, 1, 1}, {0,0,0}}),
            T = calendar:datetime_to_gregorian_seconds(Local),
            put(local_stamp, T),
            T
    end,
    TimeStamp = Unixtime + LocalStamp,
    calendar:gregorian_seconds_to_datetime(TimeStamp).

%% @doc 把Unixtime转换成星期几
-spec day_of_the_week(Unixtime :: non_neg_integer()) -> 1..7.
day_of_the_week(Unixtime) ->
    {{Y, M, D}, _} = seconds_to_datetime(Unixtime),
    day_of_the_week(Y, M ,D).
%%     BaseT = 1420387200,  %% 2015/1/5 0:0:0 星期一 0点
%%     WeekSes = 604800,
%%     case Unixtime >= BaseT of 
%%         true -> ((Unixtime - BaseT) rem WeekSes) div 86400 + 1;
%%         false -> 7 - ((BaseT - Unixtime) rem WeekSes) div 86400
%%     end.

%% @doc 把日期转换成星期几
-spec day_of_the_week(Year :: non_neg_integer(), Month :: non_neg_integer(), Day :: non_neg_integer()) -> 1..7.
day_of_the_week(Year, Month, Day) ->
    (date_to_gregorian_days(Year, Month, Day) + 5) rem 7 + 1.

%% ========================================
%% 内部方法
%% ========================================
%% 计算离0000年1月1日到现在有多少天
date_to_gregorian_days(Year, Month, Day) when is_integer(Day), Day > 0 ->
    Last = last_day_of_the_month(Year, Month),
    if
        Day =< Last ->
            dy(Year) + dm(Month) + df(Year, Month) + Day -1
    end.

%% 计算指定年月有多少天 -> 28..31
last_day_of_the_month(Year, Month) when is_integer(Year), Year >= 0 ->
    do_last_day_of_the_month(Year, Month).
do_last_day_of_the_month(_, 4) -> 30;
do_last_day_of_the_month(_, 6) -> 30;
do_last_day_of_the_month(_, 9) -> 30;
do_last_day_of_the_month(_, 11) -> 30;
do_last_day_of_the_month(Y, 2) ->
    case is_leap_year(Y) of
        true -> 29;
        false -> 28
    end;
do_last_day_of_the_month(_, M) when is_integer(M), M>0, M<13 -> 31.

%% 是否闰年 -> true | false
is_leap_year(Year) when Year rem 4 =:= 0, Year rem 100 > 0 ->
    true;
is_leap_year(Year) when Year rem 400 =:= 0 ->
    true;
is_leap_year(_) -> false.

%% Days in previous years
dy(Year) when Year =< 0 -> 0;
dy(Year) ->
    X = Year - 1,
    (X div 4) - (X div 100) + (X div 400) + X * ?DAYS_PER_YEAR + ?DAYS_PER_LEAP_YEAR.

%% Returns the total number of days in all months preceeding month, for an oridinary year
dm(1) -> 0;
dm(2) -> 31;
dm(3) -> 59;
dm(4) -> 90;
dm(5) -> 120;
dm(6) -> 151;
dm(7) -> 181;
dm(8) -> 212;
dm(9) -> 243;
dm(10) -> 273;
dm(11) -> 304;
dm(12) -> 334.

%% Accounts for an extra day in February if Year is a leap year, and if Month > 2
df(_, Month) when Month < 3 -> 0;
df(Year, _) ->
    case is_leap_year(Year) of
        true -> 1;
        false -> 0
    end.
%% @doc 时间解析
-spec parse_time(term()) -> non_neg_integer().
parse_time({day, OffSet}) -> unixtime(zero) + OffSet;
parse_time({open_time, T}) -> env:srv_open_time() + T * 86400;
parse_time({open_time, T, OffSet}) -> env:srv_open_time() + T * 86400 + OffSet;
parse_time({merge_time, T}) -> env:get(merge_time) + T * 86400;
parse_time({merge_time, T, OffSet}) -> env:get(merge_time) + T * 86400 + OffSet;
parse_time(T) when is_integer(T) -> T;
parse_time({Year, Month, Day, Hour, Minute, Second}) -> datetime_to_seconds({{Year, Month, Day}, {Hour, Minute, Second}});
parse_time({{Year, Month, Day}, {Hour, Minute, Second}}) -> datetime_to_seconds({{Year, Month, Day}, {Hour, Minute, Second}});
parse_time(T) ->
    io:format("错误的时间格式：~w", [T]),
    0.

%% @doc 双时间解析 
-spec parse_time(any(), any()) -> {non_neg_integer(), non_neg_integer()}.
parse_time({week, Sday, SOffSet}, {week, Eday, EOffSet}) ->
    Week = calendar:day_of_the_week(date()),
    ZeroT = date:unixtime(zero),
    %% ?INFO("Sday:~w Eday:~w Week:~w", [Sday, Eday, Week]),
    if 
        %% 非跨周情况
        Sday =< Eday andalso Week >= Sday andalso Week =< Eday -> %% 如当前是周六 要求是周五到周日
            {ZeroT - (Week - Sday) * 86400 + SOffSet, ZeroT + (Eday - Week) * 86400 + EOffSet};
        Sday =< Eday andalso Week < Sday -> %% 如当前是周二 要求是周六到周日
            {ZeroT + (Sday - Week) * 86400 + SOffSet, ZeroT + (Eday - Week) * 86400 + EOffSet};
        Sday =< Eday -> %% 如当前是周六 要求是周二到周三 取下一周
            {ZeroT + (Sday + 7 - Week) * 86400 + SOffSet, ZeroT + (Eday + 7 - Week) * 86400 + EOffSet};
        %% 跨周情况 Sday > Eday
        Week =< Sday andalso Week > Eday -> %% 如当前是周五 要求是周六到周二 
            {ZeroT + (Sday - Week) * 86400 + SOffSet, ZeroT + (Eday + 7 - Week) * 86400 + EOffSet};
        Week >= Sday andalso Week > Eday -> %% 如当前是周五 要求是周四到周二
            {ZeroT - (Week - Sday) * 86400 + SOffSet, ZeroT + (Eday + 7 - Week) * 86400 + EOffSet};
        true -> %% 如当前是周一 要求是周六到周二 跨周情况
            {ZeroT - (Week + 7 - Sday) * 86400 + SOffSet, ZeroT + (Eday - Week) * 86400 + EOffSet}
    end;
parse_time(StartT, EndT) ->
    {parse_time(StartT), parse_time(EndT)}.

%% 时间戳转换成字符串描述带时分秒
seconds_to_all_string(UnixTime) when is_integer(UnixTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = seconds_to_datetime(UnixTime),
    lists:concat([Year, "年", Month, "月", Day, "日", Hour, "时", Minute, "分", Second, "秒"]);
seconds_to_all_string(_) -> "".

%% 时间戳转换成字符串描述不带时分秒
seconds_to_string(UnixTime) when is_integer(UnixTime) ->
    {{Year, Month, Day}, _} = seconds_to_datetime(UnixTime),
    lists:concat([Year, "年", Month, "月", Day, "日"]);
seconds_to_string(_) -> "".


get_localtime_raw_string() ->
    %% DATETIME
    %% YYYYMMDDHHMMSS'格式来显示DATETIME值，
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[Year, Month, Day, Hour+8, Minute, Second])).