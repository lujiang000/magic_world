%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 限时任务奖励
-module(mission_time_limit_reward_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40].


%% 根据id获取一条数据
%% 1 奖励id
%% 2 道具类型
%% 3 奖励数量
get_data(1) -> {ice,2};
get_data(2) -> {horn,2};
get_data(3) -> {horn,3};
get_data(4) -> {rage,2};
get_data(5) -> {trumpet,2};
get_data(6) -> {trumpet,3};
get_data(7) -> {trumpet,4};
get_data(8) -> {trumpet,5};
get_data(9) -> {locking,2};
get_data(10) -> {locking,3};
get_data(11) -> {auto,1};
get_data(12) -> {coin,1000};
get_data(13) -> {coin,1500};
get_data(14) -> {coin,1880};
get_data(15) -> {coin,3880};
get_data(16) -> {coin,5000};
get_data(17) -> {coin,6660};
get_data(18) -> {coin,8880};
get_data(19) -> {coin,16660};
get_data(20) -> {coin,18880};
get_data(21) -> {gold,5};
get_data(22) -> {gold,10};
get_data(23) -> {gold,20};
get_data(24) -> {gold,40};
get_data(25) -> {gold,80};
get_data(26) -> {gold,150};
get_data(27) -> {tel_fare,2};
get_data(28) -> {tel_fare,5};
get_data(29) -> {tel_fare,8};
get_data(30) -> {tel_fare,10};
get_data(31) -> {tel_fare,12};
get_data(32) -> {tel_fare,15};
get_data(33) -> {tel_fare,18};
get_data(34) -> {tel_fare,20};
get_data(35) -> {red_bag,2};
get_data(36) -> {red_bag,5};
get_data(37) -> {red_bag,10};
get_data(38) -> {red_bag,12};
get_data(39) -> {red_bag,15};
get_data(40) -> {red_bag,20}.

