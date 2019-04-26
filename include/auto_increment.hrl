
%% 自动增长数据
-record(auto_increment, {
        key                       %% 唯一标识
        ,value = 0                %% 自动增长id
    }
).


%% 自动增长数据配置
-record(auto_config, {
        key                  %% 唯一标识
        ,db_name             %% 数据库名称
        ,value               %% 要查找的数据字段
        ,define              %% 默认初始值
    }
).


%% 玩家id初始值
-define(role_start_id, 1000000).

%% 配置列表
-define(auto_key_list, [
        #auto_config{key = role, db_name = role, value = role_id, define = ?role_start_id}
        ,#auto_config{key = charge, db_name = charge_log, value = id, define = 1}
    ]).
