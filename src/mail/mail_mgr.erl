%%----------------------------------------------------
%% 邮件系统管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(mail_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,send/6
        ,get_receive/1
        ,delete/2
        ,delete_all/1
        ,read/2
        ,get_items/2
        ,get_all_items/1
        ,recieve_mail/2
        ,login/1
        ,send_all/5
        ,do_conditions_sql/2
        ,to_atom_assets/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        id = 1
    }).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

-define(mail_time, 7 * 86400).

-record(mail, {
        id = 0        %% 邮件唯一id
        ,from_id = 0
        ,to_id = 0
        ,title  = ""   %% 标题
        ,msg = ""      %% 内容
        ,items = []    %% 物品列表
        ,time = 0      %% 发送时间
        ,time_out = 0  %% 过期时间
        ,status = 0    %%0，未读，1已读，2已经领取
}).

%% 发送全服邮件
send_all(RoleId, Title, Msg, Items, Condition) ->
    Title1 = case is_integer(Title) of
        true -> 
            integer_to_list(Title);
        _ ->
            unicode:characters_to_binary(list_to_binary(Title))
    end,
    Msg1 = case is_integer(Msg) of
        true -> 
            integer_to_list(Msg);
        _ ->
            unicode:characters_to_binary(list_to_binary(Msg))
    end,
    Sql = case Condition of
        [] -> "select role_id from role";
        _ -> 
            do_conditions_sql(Condition, [])
    end,
    Items1 = [#p_assets{type = to_atom_assets(Type), num = Num}||[Type, Num] <-Items],
    Time = date:unixtime(),
    case RoleId of
        0 ->
            case db:get_all(Sql) of
                {ok, List} when is_list(List) ->
                    [send(1, RoleId1, Title1, Msg1, Items1, Time)||[RoleId1] <-List],
                    true;
                _ ->
                    false
            end;
        _ ->
            send(2, RoleId, Title1, Msg1, Items1, Time),
            true
    end.



%% 根据e_item_type
to_atom_assets(1) -> ice;
to_atom_assets(2) -> horn;
to_atom_assets(3) -> rage;
to_atom_assets(4) -> trumpet;
to_atom_assets(5) -> locking;
to_atom_assets(6) -> auto;
to_atom_assets(7) -> lollipop;
to_atom_assets(8) -> coin;
to_atom_assets(9) -> gold;
to_atom_assets(10) -> tel_fare;
to_atom_assets(11) -> red_bag;
to_atom_assets(13) -> candy;   
to_atom_assets(14) -> lolly;   
to_atom_assets(15) -> self_horn;
to_atom_assets(21) -> active_card.


%% vip等级限制
do_conditions_sql([[1, Value] | L], List) -> 
    S = lists:concat(["vip >= ", Value]),
    do_conditions_sql(L, [S | List]);
%% 充值金额限制
do_conditions_sql([[2, Value] | L], List) -> 
    S = lists:concat(["charge >= ", Value]),
    do_conditions_sql(L, [S | List]);
%% 注册时间限制 开始时间
do_conditions_sql([[3, Value] | L], List) -> 
    S = lists:concat([" regist_time >= ", Value]),
    do_conditions_sql(L, [S | List]);
%% 注册时间限制 结束时间
do_conditions_sql([[4, Value] | L], List) -> 
    S = lists:concat([" regist_time <= ", Value]),
    do_conditions_sql(L, [S | List]);

do_conditions_sql([], List) ->
    "select role_id from role where " ++ string:join(List, " and ").


%% 发送邮件
send(RoleID, RoleID1, Title, Msg, Items, Time) ->
    Mail = #mail{from_id = RoleID, to_id = RoleID1, title = Title, msg = Msg, items = Items, time = Time, time_out = Time + ?mail_time},
    ?MODULE ! {send, Mail}.

%% 登陆处理，接收邮件和清理邮件
login(Role = #role{role_id = RoleID, mail_list = MailList}) ->
    NewRole = case dets:lookup(mail, RoleID) of
        List = [#mail{} | _] ->
            [dets:delete_object(mail, Mail)||Mail <-List],
            sys_conn:pack_send(1407, #m_1407_toc{}),
            Now = date:unixtime(),
            NewList = do_add_mail(lists:reverse(lists:keysort(#mail.id, List ++ MailList)), [], Now, 50),
            Role#role{mail_list = NewList};
        _ ->
            Role
    end,
    do_coin_to_red(NewRole).

%% 好友福利金币转换红包
do_coin_to_red(Role = #role{mail_list = MailList}) ->
    NewList = coin_to_red(MailList, []),
    Role#role{mail_list = NewList}.

coin_to_red([], List) -> lists:reverse(List);
coin_to_red([Mail = #mail{status = Status, title = "邀请好友奖励", time = Time, items = [#p_assets{type = coin, num = Coin}]} | L], List)  when Time < 1538931600 andalso Status =/= 2 -> 
    coin_to_red(L, [Mail#mail{items = [#p_assets{type = red_bag, num = trunc(Coin/100)}]} | List]);
coin_to_red([Mail | L], List) ->
    coin_to_red(L, [Mail | List]).



%% 获取收到的邮件
get_receive(_Role = #role{mail_list = List}) ->
    NewList = to_p_mail(List),
    #m_1401_toc{list = NewList}.

%% 删除已读邮件
delete(Role = #role{mail_list = List}, Id) ->
    NewList = lists:keydelete(Id, #mail.id, List),
    Role#role{mail_list = NewList}.

%% 删除所有已经读的邮件
delete_all(Role = #role{}) ->
    Role#role{mail_list = []}.

%% 阅读邮件
read(Role = #role{mail_list = List}, Id) ->
    case lists:keyfind(Id, #mail.id, List) of
        Mail = #mail{status = 0}->
            NewList = lists:keyreplace(Id, #mail.id, List, Mail#mail{status = 1}),
            {ok,  Role#role{mail_list = NewList}};
        _ ->
            {false, ?error_act}
    end.

%% 领取物品
get_items(Role = #role{mail_list = List}, Id) ->
    case lists:keyfind(Id, #mail.id, List) of
        Mail = #mail{items = Items, status = Status} when Items =/= [] andalso Status =/= 2->
            {ok, NewRole} = role_lib:do_add(Role, [{Type, Num}||#p_assets{type = Type, num = Num} <-Items]),
            NewList = lists:keyreplace(Id, #mail.id, List, Mail#mail{status = 2}),
            db:exec("update mail_log set status = 1, reward_time = ? where id = ?", [date:unixtime(), Id]),
            {ok, Items, NewRole#role{mail_list = NewList}};
        _ ->
            {false, ?error_act}
    end.

%% 一键领取物品    
get_all_items(Role = #role{mail_list = List}) ->
    {Items, NewList} = do_get_item(List, [], []),
    {ok, NewRole} = role_lib:do_add(Role, [{Type, Num}||#p_assets{type = Type, num = Num} <-Items]),
    {ok, Items, NewRole#role{mail_list = NewList}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
	process_flag(trap_exit, true),
    dets:open_file(mail, [{file, "./dets/mail.dets"}, {keypos, #mail.to_id}, {type, duplicate_bag}]),
    Id = do_init(),
    State = #state{id = Id},
    erlang:send_after(date:next_diff(0, 4, 0) * 1000, self(), do_clean),   
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({send, Mail = #mail{to_id = ToId}}, State = #state{id = Id}) ->
    dets:insert(mail, Mail#mail{id = Id}),
    do_mail_log(Mail),
    case role_data:get_online_role(ToId) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, recieve_mail, [Mail#mail{id = Id}]});
        _E ->
            ok
    end,
    {noreply, State#state{id = Id + 1}};

%% 每天凌晨4点去清理还没有领取的邮件
handle_info(do_clean, State) ->
    do_clean(),
    erlang:send_after(date:next_diff(0, 4, 0) * 1000, self(), do_clean),   
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    util:close_dets(mail),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 初始化邮件的id
do_init() ->
    do_clean(),
    case db:get_one("select max(id) from mail_log") of
        {ok, undefined}  -> 1;
        {ok, Num} -> Num + 1;
        _R -> 
            exit(self(), kill),
            ?ERR("邮件初始化id读取数据库出错~w", [_R])
    end.

%% 接收邮件 保留50封
recieve_mail(Role = #role{mail_list = MailList}, Mail) ->
    sys_conn:pack_send(1407, #m_1407_toc{}),
    Now = date:unixtime(),
    NewList = do_add_mail(lists:reverse(lists:keysort(#mail.id, [Mail | MailList])), [], Now, 50),
    dets:delete_object(mail, Mail),
    {ok, Role#role{mail_list = NewList}}.


%% 转换前端数据
to_p_mail([]) -> [];
to_p_mail(List) when is_list(List) ->
    [to_p_mail(Mail) || Mail <- List];
to_p_mail(#mail{id = Id, title = Title, msg = Msg, items = Items, time = Time, time_out = TimeOut, status = Status}) ->
    #p_mail{id = Id, title = Title, msg = Msg, items = Items, time = Time, timeout = TimeOut, status = Status}.


do_get_item([], Items, List) -> 
    NewItems = do_sort(Items, []),
    {NewItems, List};
do_get_item([Mail = #mail{status = 0, items = []} | L], Items, List) -> 
    do_get_item(L, Items, [Mail#mail{status = 1} | List]);
do_get_item([Mail = #mail{id = Id, status = 0, items = Items1} | L], Items, List) -> 
    db:exec("update mail_log set status = 1, reward_time = ? where id = ?", [date:unixtime(), Id]),
    do_get_item(L, Items1 ++ Items, [Mail#mail{status = 2} | List]);
do_get_item([Mail = #mail{id = Id, status = 1, items = Items1} | L], Items, List) when Items1 =/= []-> 
    db:exec("update mail_log set status = 1, reward_time = ? where id = ?", [date:unixtime(), Id]),
    do_get_item(L, Items1 ++ Items, [Mail#mail{status = 2} | List]);
do_get_item([Mail = #mail{} | L], Items, List) -> 
    do_get_item(L, Items, [Mail| List]).

do_sort([], List) -> List;
do_sort([P = #p_assets{type = Type, num = Num} | L], List) -> 
    List1 = case lists:keyfind(Type, #p_assets.type, List) of
        #p_assets{num = Num1} ->
            lists:keyreplace(Type, #p_assets.type, List, P#p_assets{num = Num + Num1});
        _ ->
            [P | List]
    end,
    do_sort(L, List1).

%% 入mysql日志
do_mail_log(#mail{from_id = FromId, to_id = ToId, items = Items, time = Time}) ->
    log_db:log(mail_log, insert, [FromId, ToId, util:term_to_string(Items), Time]).


%% 初始化处理
do_clean() ->
    Now = date:unixtime(),
    Fun = fun(Mail = #mail{id = Id, time_out = Ts}) when Ts < Now ->
            dets:delete_object(mail, Mail),
            db:exec("update mail_log set status = 2, delete_time = ? where id = ?", [date:unixtime(), Id]),
            continue;
        (_) ->
            continue
    end,
    dets:traverse(mail, Fun),
    ok.

%% 增加新邮件处理,最多50封
do_add_mail([], List, _, _Num) ->
    List;
do_add_mail(List1, List, _, 0) ->
    [db:exec("update mail_log set status = 2, delete_time = ? where id = ?", [date:unixtime(), Id])|| #mail{id = Id}<-List1],
    List;
do_add_mail([Mail = #mail{time_out = Time} | L], List, Now, Num) when Time >= Now->
    do_add_mail(L, [Mail | List], Now, Num - 1);
do_add_mail([_Mail = #mail{id = Id} | L], List, Now, Num) ->
    db:exec("update mail_log set status = 2, delete_time = ? where id = ?", [date:unixtime(), Id]),
    do_add_mail(L, List, Now, Num).


