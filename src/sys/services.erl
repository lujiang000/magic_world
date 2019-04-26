%% 服务配置数据
%%
%% @author weichengjun
%% @end
%%----------------------------------------------------
-module(services).
-export([
        config/1
        ,get/1
    ]
).

-include("service.hrl").

%% shell节点
-spec cfg(atom()) -> {ok, list()} | {error, undefined}.
cfg(shell) ->
    {ok, []};

%% 中央服
cfg(center) ->
    {ok, []};

%% 游戏区
cfg(local) ->
    {ok, [role_charge_exchange_account, friend_mgr, functions_mgr,  role_charge_mgr, role_black, role_account_mgr, task_mgr, setting_mgr, account_mgr, animal_account_mgr, weixin_mgr, shop,  mail_mgr, boradcast_mgr, animal_mgr, log_mgr, rank_mgr, role_data,  auto_increment, web_mgr, sup_acceptor, sys_listener]};


cfg(_) ->
    {error, undefined}.

%% @doc 获取指定服务的配置数据
-spec get(atom()) -> {ok, #service{}} | {error, undefined}.
get(sup_acceptor = Id) ->
    {ok, #service{
            id = Id
            ,name = "acceptor监控树"
            ,mfa = {sup_acceptor, start_link, []}
            ,type = supervisor
        }
    };

get(sys_listener = Id) ->
    {ok, #service{
            id = Id
            ,name = "socket监听服务"
            ,mfa = {sys_listener, start_link, []}
            ,depend_on = [sup_acceptor]
        }
    };

get(auto_increment = Id) ->
    {ok, #service{
            id = Id
            ,name = "全服自增长id"
            ,mfa = {auto_increment, start_link, []}
        }
    };
get(role_charge_exchange_account = Id) ->
    {ok, #service{
            id = Id
            ,name = "个人每日充值兑换日志"
            ,mfa = {role_charge_exchange_account, start_link, []}
        }
    };

get(role_black = Id) ->
    {ok, #service{
            id = Id
            ,name = "封号处理"
            ,mfa = {role_black, start_link, []}
        }
    };

get(great_match_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "大奖赛系统"
            ,mfa = {great_match_mgr, start_link, []}
        }
    };

get(functions_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "付费功能管理"
            ,mfa = {functions_mgr, start_link, []}
        }
    };


get(role_charge_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "充值缓冲进程"
            ,mfa = {role_charge_mgr, start_link, []}
        }
    };

get(great_account_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "大奖赛统计系统"
            ,mfa = {great_account_mgr, start_link, []}
        }
    };

get(setting_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "游戏相关设置"
            ,mfa = {setting_mgr, start_link, []}
        }
    };

get(area_account_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "竞技场统计系统"
            ,mfa = {area_account_mgr, start_link, []}
        }
    };

get(role_account_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "人物统计系统"
            ,mfa = {role_account_mgr, start_link, []}
        }
    };

get(task_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "每日任务"
            ,mfa = {task_mgr, start_link, []}
        }
    };

get(boradcast_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "广播系统"
            ,mfa = {boradcast_mgr, start_link, []}
        }
    };
get(weixin_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "微信全局值系统"
            ,mfa = {weixin_mgr, start_link, []}
        }
    };
get(mail_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "邮件系统"
            ,mfa = {mail_mgr, start_link, []}
        }
    };
get(animal_account_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "动物园统计系统"
            ,mfa = {animal_account_mgr, start_link, []}
        }
    };
get(account_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "其他数据统计系统"
            ,mfa = {account_mgr, start_link, []}
        }
    };
get(robot_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "机器人系统"
            ,mfa = {robot_mgr, start_link, []}
        }
    };
get(friend = Id) ->
    {ok, #service{
            id = Id
            ,name = "好友系统"
            ,mfa = {friend, start_link, []}
        }
    };

get(friend_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "新好友系统"
            ,mfa = {friend_mgr, start_link, []}
        }
    };

get(shop = Id) ->
    {ok, #service{
            id = Id
            ,name = "神秘商店系统"
            ,mfa = {shop, start_link, []}
        }
    };

get(area_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "竞技场系统"
            ,mfa = {area_mgr, start_link, []}
        }
    };

get(role_data = Id) ->
    {ok, #service{
            id = Id
            ,name = "角色数据存取服务"
            ,mfa = {role_data, start_link, []}
        }
    };


get(chat_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "聊天服务器"
            ,mfa = {chat_mgr, start_link, []}
        }
    };

get(log_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "日志系统"
            ,mfa = {log_mgr, start_link, []}
        }
    };


get(web_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "后台系统"
            ,mfa = {web_mgr, start_link, []}
            ,depend_on = []
        }
    };
get(rank_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "排行系统"
            ,mfa = {rank_mgr, start_link, []}
            ,depend_on = []
        }
    };
get(animal_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "动物园系统"
            ,mfa = {animal_mgr, start_link, []}
            ,depend_on = []
        }
    };
 
get(_Name) ->
    {error, undefined}.

%% @doc 读取默认配置
-spec config(atom()) -> {ok, list()} | {error, undefined}.
config(Type) ->
    case cfg(Type) of
        {ok, L} -> {ok, parse(L, Type, [])};
        Else -> Else
    end.

%% 统一处理成带参数的格式
parse([], _Type, L) -> lists:reverse(L);
parse([Id = team_hall_mgr | T], Type, L) -> parse(T, Type, [{Id, [Type]} | L]);
parse([Id = cross_control_mgr | T], Type, L) -> parse(T, Type, [{Id, [Type]} | L]);
parse([Id = c_rank_mgr | T], Type = center, L) -> parse(T, Type, [{Id, [center]} | L]);
parse([Id | T], Type, L) -> parse(T, Type, [{Id, []} | L]).
