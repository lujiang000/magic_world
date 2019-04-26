%% 按固定格式输出普通信息到控制台
-define(INFO(Msg), logger:info(Msg, [], ?MODULE, ?LINE)).
-define(INFO(F, A), logger:info(F, A, ?MODULE, ?LINE)).
%% 按固定格式输出错误信息到控制台
-define(ERR(Msg), logger:error(Msg, [], ?MODULE, ?LINE)).
-define(ERR(F, A), logger:error(F, A, ?MODULE, ?LINE)).

-define(game_time, 8 * 3600).  %% 每天以8点为结算时间


%% 钻石类型
-define(gold_gift, 1).
-define(gold_up, 2).
-define(gold_goods, 3).
-define(gold_code, 4).
-define(gold_charge, 5).
-define(gold_bp, 6).
-define(gold_achievement, 7).
-define(gold_bp_back, 8).
-define(gold_register, 9). %% 通过分享链接进来，注册送分
-define(gold_up_get, 10). %% 玩家点赞获得的钻石
-define(coin_red_bag, 11). %% 玩家发红包记录 用于分账使用
-define(gold_guide_up, 12). %% 新手点赞
-define(gold_title, 13). %% 获得称号钻石记录
-define(gold_song, 14). %% 点歌
-define(gold_song_back, 15). %% 点歌取消

%% 充值类型
-define(charge_coin, 1).  %% 金币充值
-define(charge_gold, 2).  %% 钻石充值
-define(charge_first, 3). %% 首冲礼包
-define(charge_gift, 4).  %% 贵族礼包
-define(charge_active, 5).  %% 活动礼包

%% 金币产出类型
-define(invite_coin, 101).    %% 邀请
-define(share_coin, 102).     %% 分享
-define(alms_coin, 103).      %% 救济金
-define(vip_alms_coin, 104).  %% vip救济金
-define(open_fire, 105).      %% 解锁
-define(area_coin, 106).      %% 竞技场
-define(login_coin, 107).     %% 登陆转盘
-define(vip_login_coin, 108).     %% vip登陆转盘
-define(friend_coin, 109).     %% 好友奖励
-define(id_card_coin, 110).     %% 身份验证奖励
-define(guide_task_coin, 111).     %%新手任务奖励 
-define(guide_login_coin, 112).     %%新手登陆奖励 
-define(gift_code_coin, 113).     %%礼品码奖励 


%% 游戏设置
-define(setting_animal, 1). %% 设置是否带红包
-define(setting_shop, 2).  %% 设置神秘商店
-define(setting_animal_pre, 3).  %% 设置返奖率
-define(setting_animal_white_pre, 4).  %% 设置白名单返奖率
-define(setting_friend, 5).  %% 设置好友信息
-define(setting_redbag, 6).  %% 设置红包最低提现金额
-define(setting_animal_black_pre, 7).  %% 设置黑名单单返奖率
-define(setting_lollipop_pre, 8).  %% 设置棒棒糖掉落概率
-define(setting_kill_rank, 9).  %% 设置击日杀榜奖励
-define(setting_kill_rank_on, 10).  %% 设置击日击杀榜是否开启
-define(setting_kill_rank_week, 11).  %% 设置击周击杀榜奖励
-define(setting_kill_rank_week_on, 12).  %% 设置击周击杀榜是否开启
-define(setting_new_year, 13).  %% 设置新年活动
-define(setting_coin_tree, 14).  %% 设置摇钱树活动
-define(setting_gold_pick, 15).  %% 金猪活动 
-define(setting_girl, 16).  %% 38女神节日

%% 好友默认设置
-define(friend_setting, [[1, 5, 1], [2, 2, 0], [3, 2, 0], [4, 1, 0]]).
-define(redbag_setting, 10).


-define(role_max_num, 10000000). %% 每个服最大id数量

-define(screen_id, 1). %% 大屏id

-define(animal_pre, 0.94).   %% 返奖率 随着VIP等级需要调整

-define(animal_white_pre, 1).   %% 白名单返奖率
-define(animal_black_pre, 0.8).   %% 黑名单单返奖率
-define(lollipop_pre, 0.02).  %% 棒棒糖掉落概率

%%　修改以下数据需要玩家下线才会生效
%% 登陆公众号
-define(LoginAppID, "wx6550285d3b5d797f").
-define(LoginAppSecret, "b5cb3a1b3b0891aca88106175ac7f4d2").
%% 唯卓
-define(RedMachId, "1511880951").
-define(RedMachKey, "d9oPC3A6VI17J7vxtB3v98DGqUw12N7J").
-define(RedAppId, "wx5f94b1370d1abdb8").
-define(RedAppSecret, "6c7363f45828132c63f2a05577b7d74f").
%% 中传
%% 兴业
%%-define(MachId, "403580132115").
%%-define(MachKey, "13a6bbc11918f778471ae52a5dda8209").
%% 晋商
-define(MachId, "174520003257").
-define(MachKey, "7a465c9d4eb2c9f37a3e2bcfc1e65933").
-define(PayAppId, "wxc1384a875174cb3c").
-define(PayAppSecret, "72290d6096f0eb490fc29389c04c46f3").

%% int最大数值
-define(int_max_num, 2147483647).

-record(reward, {
        role_id          %% 人物id
        ,type            %% 日志类型 
        ,value_type      %% 值的类型，gold,coin
        ,value           %% 值
    }
).

-define(gm_id, [1062011, 1062016, 1062021, 1062018, 1062013, 1062132, 1003063, 1036209, 1021078, 1051301]).


%% 礼包码规则
-define(gift_code, "6d42bf5f").
-define(gift_code_flag,  1).
-define(gift_code_reward,  [{coin, 10000}, {gold, 10}, {red_bag, 10}]).



