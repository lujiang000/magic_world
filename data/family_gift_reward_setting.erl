%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 联盟礼物奖励
-module(family_gift_reward_setting).
-export([
		get_data/1
	]).

%% 根据id获取一条数据
%% 1 奖励id
%% 2 宝箱奖励
%% 3 钥匙能量
get_data(1) -> {[{coin,100},{gold,10}],0};
get_data(2) -> {[{coin,100},{gold,10}],0};
get_data(3) -> {[{coin,100},{gold,10}],0};
get_data(4) -> {[{coin,100},{gold,10}],0};
get_data(5) -> {[{coin,100},{gold,10}],0};
get_data(100) -> {[{coin,100},{gold,10}],1};
get_data(101) -> {[{coin,100},{gold,10}],2};
get_data(102) -> {[{coin,100},{gold,10}],3};
get_data(103) -> {[{coin,100},{gold,10}],4};
get_data(104) -> {[{coin,100},{gold,10}],5};
get_data(105) -> {[{coin,100},{gold,10}],6};
get_data(200) -> {[{coin,100},{gold,10}],8};
get_data(201) -> {[{coin,100},{gold,10}],9};
get_data(202) -> {[{coin,100},{gold,10}],10};
get_data(203) -> {[{coin,100},{gold,10}],11};
get_data(204) -> {[{coin,100},{gold,10}],12}.

