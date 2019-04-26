%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 大奖赛周榜
-module(arena_rank_weekly_reward_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1].


%% 根据id获取一条数据
%% 1 名次
%% 2 奖励列表
get_data(1) -> {[{lollipop,100}]}.

