%%----------------------------------------------------
%% @doc 随机数种子服务器
%%  weichengjun
%% @end
%%----------------------------------------------------
-module(sys_rand).
-behaviour(gen_server).
-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    get_seed/0
    ,rand/1
    ,rand/2
    ,rand_list/1
    ,rand_list/2
    ,test/1
]).
-record(state, {seed}).

%% --- 系统调用 ---------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --- 对外接口 ---------------------------------
%% @spec get_seed() -> {integer(), integer(), integer()}
%% @doc 取得一个随机数种子
get_seed() ->
    catch gen_server:call(?MODULE, get_seed).

%% 产生一个介于Min到Max之间的随机整数
rand(Max) ->
    rand(1, Max).

rand(Min, Min) -> Min;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    case get('#rand_seed') of
        undefined ->
            RandSeed = get_seed(),
            rand:seed(exs1024, RandSeed),
            put('#rand_seed', RandSeed);
        _ ->
	   skip
    end,
    M = Min - 1,
    rand:uniform(abs(Max - M)) + M.


%% 从一个list中随机取出一项
%% null | term()
rand_list([]) -> null;
rand_list([I]) -> I;
rand_list(List) -> 
    Len = length(List),
    Index = rand(Len),
    lists:nth(Index, List).

%% @doc 从一个list中按各项权重值随机取出一项 每项为tuple()
-spec rand_list(List::[tuple()], Pos::pos_integer()) -> undefined | tuple().
rand_list([], _Pos) -> undefined;
rand_list([I], _Pos) -> I;
rand_list(List, Pos) ->
    Sum = lists:sum([element(Pos, I) || I <- List]),
    RandVal = rand(Sum),
    get_rand_tuple(List, Pos, RandVal).
get_rand_tuple([H | T], Pos, RandVal) ->
    Rand = element(Pos, H),
    case RandVal =< Rand of
        true -> H;
        false -> get_rand_tuple(T, Pos, RandVal - Rand)
    end.

%% @doc 概率命中
%% @return true | false
test(Rate) ->
    R = round(Rate * 100000000),
    case rand(1, 100000000) of
        N when N =< R ->
            true;
        _ ->
            false
    end.

%% --- 服务器内部实现 ---------------------------------
%% @hidden
init([]) ->
    State = #state{},
    {ok, State}.

%% @hidden
%% 返回一个随机数组合做为其它进程的随机数种子
handle_call(get_seed, _From, State) ->
    case State#state.seed of
        undefined ->
            rand:seed(exs1024, erlang:timestamp());
        S ->
            rand:seed(exs1024, S)
    end,
    Seed = {rand:uniform(99999), rand:uniform(999999), rand:uniform(999999)},
    {reply, Seed, State#state{seed = Seed}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


