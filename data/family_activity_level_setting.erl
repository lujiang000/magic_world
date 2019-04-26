%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 家族联盟活动等级
-module(family_activity_level_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5].


%% 根据id获取一条数据
%% 1 等级
%% 2 免费任务的次数
%% 3 最高可以领取的奖励等级
get_data(1) -> {6,8};
get_data(2) -> {7,11};
get_data(3) -> {8,14};
get_data(4) -> {9,17};
get_data(5) -> {10,20}.

