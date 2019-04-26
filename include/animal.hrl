%% 动物园房间最多人数
-define(animal_max_num, 5).

%% 动物园随机数
-define(animal_rand_num, 100000000).

%% 动物园房间结构
-record(animal_room, {
        id = 0
        ,type = 0
        ,num = 0
        ,pid 
    }
).
%% 动物园人物信息
-record(animal_role, {
        role_id = 0
        ,name = ""
        ,icon = ""
        ,vip = 0
        ,pid 
        ,socket_pid 
        ,skill_id = []
        ,effect = 1   %% 狂暴倍数
        ,vip_effect = 0 %% vip特效
    }
).

%% 动物基本信息
-record(animal_base, {
        id = 0              %% 唯一id
        ,name               %% 动物名字
        ,base_id = 0        %% 基础id
        ,end_time = 0       %% 剩余总步数
        ,rate = 0           %% 倍率
        ,route_id = 0       %% 线路唯一id
        ,post = 0           %% 当前位置
        ,is_notice = 0      %% 0不广播，1广播
        ,win = 0            %% 盈利
        ,pre = 0            %% 出现概率
        ,status = 0         %% 0正常，1冰冻
        ,drop_list = []     %% 掉落的列表
        ,item_list = []     %% 可以掉落的列表
        ,bonus = 0          %% 是否是彩金动物 0否，1是
        ,is_horn = 0        %% 是否是召唤的动物，0否 
        ,xy = {0, 0}        %% 当前坐标
        ,red_bag = 0        %% 是否有红包，0 没有，1有
        ,bomber_type = 0    %% 同类型炸弹类型 
        ,self_id = 0        %% 私有大象人物id
        ,self_name = 0      %% 私有大象人物名字
    }
).

%% 动物路线
-record(animal_route, {
        id = 0              %% 唯一id
        ,post = 0           %% 当前位置
        ,time = 0           %% 总时间
        ,xy = {0, 0}        %% 初始坐标
    }
).

%% 动物园房间类型
-define(zone_type_c, civilian). %% 平民
-define(zone_type_p, petty). %% 小资
-define(zone_type_r, rich). %% 富豪
-define(zone_type_g, gold). %% 黄金
-define(zone_type_d, diamond). %% 钻石
-define(zone_type_s, single). %% 单人

%% 最多只允许一只动物在场上列表
-define(animal_only_one_list, [bomber, pikachu, elephant, type_bomber, dsy, xsx, area_bomber]).
%% 需要预警的动物列表
-define(animal_pre_notice_list, [bomber, elephant, self_elephant, gold_pick]).

%% 有红包的列表
-define(red_bag_animal_list, [hippo, lion, elephant, pikachu, self_elephant, gold_pick]).

