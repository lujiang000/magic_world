

%% 人物数据结构版本号
-define(role_var, 0).

%% 物品结构版本号
-define(role_item_var, 0).

%% 礼包结构版本号
-define(role_gift_var, 0).

%% 物品列表
-define(item_list, [ice, horn, rage, trumpet, locking, auto, lollipop]).

%% 物品结构
-record(role_item, {
        var = ?role_item_var
        ,ice = 0          %% 冰冻
        ,horn = 0         %% 号角
        ,rage = 0         %% 狂暴
        ,trumpet = 0      %% 喇叭
        ,locking = 0      %% 锁定
        ,auto = 0         %% 自动
        ,lollipop = 0     %% 棒棒糖
        ,tel_fare = 0     %% 话费
        ,red_bag = 0      %% 红包
    }
).

%% 贵族礼包结构
-record(role_gift, {
        var = ?role_gift_var
        ,buy_time = 0         %% 购买时间
        ,end_time = 0         %% 到期时间
        ,reward_time = 0      %% 领奖时间
    }
).

-define(role_task_var, 0).
-record(role_task, {
        var = ?role_task_var
        ,id = 0
        ,type = 0        %% 类型
        ,type1 = 0       %% 二级类型
        ,value = 0       %% 当前值
        ,target = 0      %% 目标值
        ,reward = []     %% 奖励
    }
).

-record(task_process, {
         id = 0          %% id
        ,type = 0        %% 类型
        ,value = 0       %% 当前值
        ,target = 0      %% 目标值
        ,finish  = 0     %% 是否已经完成 0 否，1是
    }
).


%% 人物每日任务数据
-record(role_daily_task, {
        type = 0
        ,list = []    
        ,finish = []
    }
).

%% 每条每日任务
-record(daily_task, {
        id = 0
        ,value = 0
        ,target = 0
        ,type = 0
        ,reward = []
    }
).


-define(guide_task, 0).
-record(guide_task, {
        var = ?guide_task
        ,id = 0
        ,value = 0
        ,target = 0
        ,type = 0
        ,reward = []
        ,type_1 = 0
    
    }
).

-define(great_match, 0).
-record(great_match, {
        var = ?great_match
        ,daily_times = 0           %% 每日进行的次数
        ,vip_add = 0               %% vip积分加成
        ,repeat_add = 0            %% 重复进行次数加成
        ,high_add = 0              %% 高倍积分加成
        ,task_add = 0              %% 任务积分加成
        ,base_score = 0            %% 单次基础积分
        ,once_score = 0            %% 单次总积分
        ,num = 0                   %% 剩余魔法
        ,daily_score = 0           %% 每日累计积分
        ,week_score = 0            %% 每周累计积分
    
    }
).

-record(role, {
        var = ?role_var
        ,role_id = 0
        ,name = ""
        ,pid 
        ,socket_pid
        ,icon = ""          %% 头像
        ,gold = 0           %%钻石
        ,coin = 100           %%金豆
        ,screat = 0         %%登陆密匙
        ,type = 0           %% 0正常玩家，1机器人
        ,open_id = ""
        ,ip = ""
        ,parent_id = 0            %%推荐人id
        ,loop_counter = 0         %%循环次数
        ,need_sync = false        %%是否需要保存
        ,vip = 0                  %% vip等级
        ,vip_charge = 0           %% VIP 充值
        ,sex = 0                  %% 性别
        ,regist_time = 0          %% 注册时间
        ,login_time = 0           %% 最后一次登陆时间
        ,off_time = 0             %% 最后一次离线时间
        ,charge = 0               %% 总充值额(分)
        ,room_type = 0            %% 动物园房间类型
        ,room_pid                 %% 房间pid
        ,item = #role_item{}      %% 玩家物品信息
        ,profit = 0               %% 每日盈利
        ,status = 0               %% 0大厅，1
        ,id_card = ""             %% 身份证号
        ,true_name = ""           %% 真实姓名
        ,use_coin = 1             %% 玩游戏的炮等级
        ,bonus_pool = 0           %% 彩金池
        ,bonus_num = 5            %% 已经打中彩金动物数量
        ,bonus_reward = -1         %% 已经领取的彩金次数 用于新手指引
        ,gift = #role_gift{}      %% 贵族礼包
        ,mail_list = []           %% 收到的邮件列表
        ,send_log  = []           %% 赠送记录
        ,exchange_log  = []       %% 兑换记录
        ,use_log  = []            %% 使用记录
        ,daily_value = []         %% 每日次数记录{key, value}
        ,first_gift = 0           %% 是否购买1元礼包，0否，1是
        ,skill_list = []          %% 技能列表
        ,sign = ""                %% 个性签名
        ,red_openid = ""          %% 发红包的openid
        ,pay_openid = ""          %% 支付的opendid
        ,task = {[], []}          %% 任务成就，{未完成的，已经完成的}
        ,phone = ""               %% 手机号
        ,guide = []               %% 已经完成的新手指引列表
        ,guide_gift = 0           %% 已经领取的新手指引礼包个数
        ,guide_task = #guide_task{}  %% 新手指引任务
        ,guide_gift_time = 0      %% 领取礼包的时间
        ,off = 0                  %% 是否下线，0否，1是
        ,first_charge = 0         %% 是否第一次充值
        ,exchange = 0             %% 总兑换多少钱（分）
        ,luck = 0                 %% 幸运值 充值1块钱一点幸运值，满200幸运值5%的概率获得幸运次数
        ,luck_num = 0             %% 幸运次数
        ,daily_task = {[], [], 0}    %% 每日任务列表 已完成和未完成的列表
        ,channel = 0              %% 渠道商id
        ,charge_reward_tomorrow = 0     %% vip充值返利 明天可以领取值
        ,charge_reward_today = 0        %% vip充值返利 今日可以领取值
        ,candy = 0                      %%  小棒棒糖
        ,lolly = 0                      %%  中棒棒糖
        ,hit_num = 0                    %%  累计打动物次数
        ,animal_flag = 0                %%  99白名单，98薅羊毛
        ,phone_screat = ""              %%  手机密码
        ,gift_code = 0                  %%  礼包码是否领取
        ,coin_tree_time = 0             %% 摇钱树领取的时间
        ,self_horn = 0                  %% 私有号角
        ,talk_list = []                  %% 最近聊天
        ,daily_kill = 0                 %% 每日击杀值
        ,week_kill = 0                  %% 周击杀值
        ,xin_card = 0                   %% 新字卡
        ,nian_card = 0                  %% 年字卡
        ,kuai_card = 0                  %% 快字卡
        ,le_card = 0                    %% 乐字卡
        ,vip_effect = 0                 %% vip效果
        ,gold_pick = 0                  %% 金猪
        ,active_card = 0                %% 活动抽奖卡片
        ,achievement = []               %% 成就列表
        ,daily_sign = {0, 0}            %% 每日签到数据，{天数，签到时间}
        ,week_task = []                 %% 每周任务
        ,subscribe = 0                  %% 是否关注
        ,subscribe_reward = 0           %% 是否领取关注奖励
    }
).




%% 人物技能列表
-record(role_skill, {
        var = 0
        ,type = 0
        ,end_time = 0
        ,effect = 0
    }
).

%% 事件类型
-define(status_normal, 0).   %%大厅
-define(status_zone, 1).     %%动物园
-define(status_area, 2).     %%竞技场免费场
-define(status_area_sign, 3).     %%竞技场免费场报名状态
-define(status_great_match, 4).     %%大奖赛


%% 每日次数类型
-define(daily_login, 1).  %% 登陆奖励
-define(daily_alms, 2).   %% 救济金VIP
-define(daily_send, 3).   %% 赠送道具
-define(daily_vip_login,  4).  %% Vip登陆奖励
-define(daily_area_free,  5).  %% 获取竞技免费场次数
-define(daily_phone,  6).  %% 获取验证码次数
-define(daily_share,  7).  %% 获取分享次数
-define(daily_free_alms, 8).   %% 救济金普通
-define(daily_red_bag, 9).   %% 每日领取红包的次数
-define(daily_mid_act, 10).   %% 每日中秋节领取次数
-define(daily_coin_treee, 11).   %% 每日摇钱树次数
-define(daily_great_match, 12).   %% 每日大奖赛次数
-define(daily_girl_active, 13).   %% 每日女生节购买次数


%% 日志类型
-define(coin_cost_hit, 1).           %% 打动物
-define(coin_cost_charge, 2).        %% 充值
-define(coin_cost_charge_send, 3).   %% 充值赠送

%% 成就类型
-define(achievement_hunt, 1). %% 打动物
-define(achievement_login, 2). %% 登陆
-define(achievement_magic, 3). %% 解锁魔法

-define(achievement_type_list, [?achievement_hunt, ?achievement_login, ?achievement_magic]).


%% 在线玩家数据结构
-record(online_role, {
        role_id
        ,name = ""
        ,pid
        ,socket_pid
        ,icon = ""
        ,screat = 0
        ,gold = 0
        ,coin = 0
        ,vip = 0
        ,sign = ""
    }
).

%% vip福利
-record(vip_welfare, {
        leve = 0           %% vip等级
        ,alms = 0          %% 救济金
        ,send = 0          %% 赠送次数
        ,login = 0         %% 登陆奖励次数
        ,charge_gold = 0   %% 充值金币赠送比例 百分比
        ,arena = 0         %% 竞技场结算加成比例  百分比
        ,max_red = 0       %% 每天最多提现次数
    }
).

%% 白名单
-record(white_role, {
        role_id
        ,name
        ,time
    }
).

