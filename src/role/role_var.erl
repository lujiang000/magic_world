%%----------------------------------------------------
%% @doc 人物版本控制
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_var).
-export([
        update_var/1
    ]).

-include("common.hrl").
-include("role.hrl").


update_var(Role = #role{var = ?role_var}) -> {ok, Role};
update_var(_Role) -> 
    ?ERR("玩家信息版本升级失败:~w", [_Role]),
    false.


