%%----------------------------------------------------
%% @doc vip 相关处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(vip).
-export([
        get_lev/1
        ,get_vip_welfare/1
    ]
).

-include("role.hrl").


%% 根据充值额（分）获取最新vip等级
get_lev(Gold) when Gold < 10000 ->    0;
get_lev(Gold) when Gold < 50000 ->    1;
get_lev(Gold) when Gold < 200000 ->   2;
get_lev(Gold) when Gold < 500000 ->   3;
get_lev(Gold) when Gold < 1000000 ->  4;
get_lev(Gold) when Gold < 3000000 ->  5;
get_lev(Gold) when Gold < 5000000 ->  6;
get_lev(Gold) when Gold < 10000000 -> 7;
get_lev(Gold) when Gold < 18000000 -> 8;
get_lev(Gold) when Gold < 26000000 -> 9;
get_lev(Gold) when Gold < 34000000 -> 10;
get_lev(Gold) when Gold < 42000000 -> 11;
get_lev(Gold) when Gold < 50000000 -> 12;
get_lev(Gold) when Gold < 58000000 -> 13;
get_lev(Gold) when Gold < 100000000 -> 14;
get_lev(_Gold) ->  15.


%% 获取vip福利
get_vip_welfare(0) -> #vip_welfare{max_red = 1};
get_vip_welfare(1) -> #vip_welfare{leve = 1, login = 1, charge_gold = 2, max_red = 3};
get_vip_welfare(2) -> #vip_welfare{leve = 2, login = 1, send = 1, charge_gold = 3, max_red = 4};
get_vip_welfare(3) -> #vip_welfare{leve = 3, login = 1, send = 2, alms = 10000, charge_gold = 4, max_red = 5};
get_vip_welfare(4) -> #vip_welfare{leve = 4, login = 1, send = 5, alms = 20000, charge_gold = 5, max_red = 5};
get_vip_welfare(5) -> #vip_welfare{leve = 5, login = 1, send = 10, alms = 40000, charge_gold = 6, max_red = 6};
get_vip_welfare(6) -> #vip_welfare{leve = 6, login = 2, send = 20, alms = 100000, charge_gold = 8, max_red = 6};
get_vip_welfare(7) -> #vip_welfare{leve = 7, login = 2, send = 50, alms = 250000, charge_gold = 9, max_red = 7};
get_vip_welfare(8) -> #vip_welfare{leve = 8, login = 3, send = 100, alms = 500000, charge_gold = 10, max_red = 7};
get_vip_welfare(9) -> #vip_welfare{leve = 9, login = 3, send = 200, alms = 800000, charge_gold = 10, max_red = 8};
get_vip_welfare(10) -> #vip_welfare{leve = 10, login = 3, send = 250, alms = 1000000, charge_gold = 11, max_red = 8};
get_vip_welfare(11) -> #vip_welfare{leve = 11, login = 3, send = 300, alms = 1300000, charge_gold = 11, max_red = 9};
get_vip_welfare(12) -> #vip_welfare{leve = 12, login = 3, send = 350, alms = 1600000, charge_gold = 12, max_red = 9};
get_vip_welfare(13) -> #vip_welfare{leve = 13, login = 3, send = 400, alms = 1800000, charge_gold = 12, max_red = 10};
get_vip_welfare(14) -> #vip_welfare{leve = 14, login = 3, send = 450, alms = 2000000, charge_gold = 13, max_red = 10};
get_vip_welfare(_) -> #vip_welfare{leve = 15, login = 3, send = 500, alms = 2300000, charge_gold = 14, arena = 7, max_red = 10}.
