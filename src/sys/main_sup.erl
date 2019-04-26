%% @author weichengjun
%% @doc @todo Add description to game_sup.


-module(main_sup).
-behaviour(supervisor).
-export([init/1]).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1]).

%% ====================================================================
%% API functions
%% ====================================================================
start_link(Type) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Type]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================

init([Type]) ->
     {ok, {
            {one_for_one, 50, 1}
            ,[
                {sys_env, {sys_env, start_link, []}, transient, 10000, worker, [sys_env]}
                ,{sys_code, {sys_code, start_link, []}, transient, 10000, worker, [sys_code]}
                ,{sys_rand, {sys_rand, start_link, []}, transient, 10000, worker, [sys_rand]}
                ,{sys_boot, {sys_boot, start_link, [Type]}, transient, 10000, worker, [sys_boot]}
            ]
        }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================


