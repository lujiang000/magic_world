-record(rank_role, {
        id = 0                  %%单个榜的唯一id
        ,role_id = 0
        ,name = ""
        ,icon = ""
        ,vip = 1
        ,sign = ""           %% 签名
        ,value1 = 0          %% 排序字段预留
        ,value2  
        ,value3 
        ,value4 
        ,value5 
    }
).

-record(rank, {
        type = 0
        ,list = []
        ,last_val = 0
        ,len = 0
    }
).

-record(rank_log, {
        type = 0
        ,list = []
        ,time = 0
        ,len = 0
    }
).

-record(rank_config, {
        min = 1            %%最低要求值
        ,len = 50            %%最大长度
        ,zone = rank_zone_1            %% 排行区号
        ,keys = [#rank_role.value1]
    }
).

-define(rank_coin, 1).           %%金豆排行榜
-define(rank_lollipop, 2).       %%棒棒糖排行榜
-define(rank_profit, 3).         %%每日盈利排行榜
-define(rank_kill, 4).           %%每日击杀排行榜
-define(rank_great_match, 5).         %%每日大奖赛排行榜
-define(rank_week_great_match, 6).         %%每周大奖赛排行榜
-define(rank_kill_week, 7).                %%每周击杀排行榜



-define(rank_reward_list, [?rank_kill, ?rank_kill_week, ?rank_great_match, ?rank_week_great_match]).  %% 需要发奖励的排行榜
