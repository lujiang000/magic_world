%%----------------------------------------------------
%% 中央服节点连接进程管理
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(sys_cross).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        center
    }).

-include("common.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Local = node(),
    Center = sys_env:get_env(center),
    case Local =:= Center of
        true -> ok;
        _ ->
            ?MODULE ! {connect_center, Center}
    end,
    net_kernel:monitor_nodes(true),
    State = #state{center = Center},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
%%    cross_gdsyxw:send_all(Node),
    ?INFO("~w connect success ~n", [Node]),
    {noreply, State};

handle_info({nodedown, Node}, State = #state{center = Node}) ->
    ?INFO("~w disconnect ~n", [Node]),
    erlang:send_after(10000, self(), {connect_center, Node}),
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    ?INFO("~w disconnect ~n", [Node]),
    {noreply, State};

handle_info({connect_center, Node}, State) ->
    case net_kernel:connect_node(Node) of
        true -> 
            ?INFO("center connect success ~w~n", [Node]);
        _Err ->
            erlang:send_after(10000, self(), {connect_center, Node})
%%            ?INFO_MSG("center connect fial ~w:~w~n", [Node, _Err])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
