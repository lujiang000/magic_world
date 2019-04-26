%%----------------------------------------------------
%% 系统启动程序
%% 
%% @author weichengjun
%% @end
%%----------------------------------------------------
-module(sys_boot).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("service.hrl").
-record(state, {}).

%%----------------------------------------------------
%% 对外接口
%%----------------------------------------------------

%% 启动
-spec start_link(atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Type) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Type], []).

%%----------------------------------------------------
%% 内部实现
%%----------------------------------------------------

init([Type]) ->
    State = #state{},
    db:init(),
    self() ! {init, Type},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({init, Type}, State) ->
    case services:config(Type) of
        {ok, Services} ->
            start_services(Services);
        _ ->
            ?ERR("服务器启动找不到配置：~w", [Type])
    end,
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

start_services([]) -> 
    ?INFO("所有服务启动完毕"),
    ok;
start_services([{Id, Args} | T]) ->
    case services:get(Id) of
        {error, undefined} -> 
            ?ERR("找不到系统配置~w", [Id]),
            {error, {service_undefined, Id}};
        {ok, #service{id = Id, mfa = {M, F, A}, restart = Restart, type = Type}} ->
            case supervisor:start_child(main_sup, {Id, {M, F, A ++ Args}, Restart, 300000, Type, [Id]}) of
                {ok, _Pid} -> start_services(T);
                {error, Reason} -> 
                    ?ERR("系统启动出错~w：~w", [Id, Reason]),
                    {error, {Id, Reason}}
            end
    end.
