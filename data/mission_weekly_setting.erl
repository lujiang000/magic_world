%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 每日任务
-module(mission_weekly_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8].


%% 根据id获取一条数据
%% 1 任务id
%% 2 数量
%% 3 奖励列表
%% 4 
get_data(1) -> {5,[{gold,10}],{sign,1}};
get_data(2) -> {15,[{gold,10}],{share,1}};
get_data(3) -> {5,[{gold,10}],{lottery, 1}};
get_data(4) -> {100,[{coin,60000}],{animal_die, lion}};
get_data(5) -> {100,[{coin,80000}],{animal_die, elephant}};
get_data(6) -> {1000,[{coin,20000}],{animal_die, all}};
get_data(7) -> {5000000,[{coin,50000}],{win_gold,1}};
get_data(8) -> {5,[{coin,20000}],{daily_task,1}}.

