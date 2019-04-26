%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 家族联盟活动家族排名奖励
-module(family_activity_rank_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5].


%% 根据id获取一条数据
%% 1 等级
%% 2 成员奖励
%% 3 盟主奖励
get_data(1) -> {[{coin,1000}],[{coin,1000}]};
get_data(2) -> {[{coin,1001}],[{coin,1001}]};
get_data(3) -> {[{coin,1002}],[{coin,1002}]};
get_data(4) -> {[{coin,1003}],[{coin,1003}]};
get_data(5) -> {[{coin,1004}],[{coin,1004}]}.

