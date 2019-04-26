%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 累计捕获动物
-module(achievement_hunt_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30].


%% 根据id获取一条数据
%% 1 任务id
%% 2 捕获数量
%% 3 道具类型(server)
%% 4 道具数量
get_data(1) -> {88,coin,500};
get_data(2) -> {188,coin,1000};
get_data(3) -> {288,coin,2000};
get_data(4) -> {388,coin,3000};
get_data(5) -> {588,coin,4000};
get_data(6) -> {888,coin,5000};
get_data(7) -> {1288,coin,6000};
get_data(8) -> {1688,coin,6500};
get_data(9) -> {1888,coin,6800};
get_data(10) -> {2088,coin,7000};
get_data(11) -> {2288,coin,7200};
get_data(12) -> {2488,coin,7500};
get_data(13) -> {2688,coin,7800};
get_data(14) -> {2888,coin,8000};
get_data(15) -> {3088,coin,8200};
get_data(16) -> {3288,coin,8400};
get_data(17) -> {3488,coin,8600};
get_data(18) -> {3688,coin,8800};
get_data(19) -> {3888,coin,9000};
get_data(20) -> {4088,coin,10000};
get_data(21) -> {4288,coin,11000};
get_data(22) -> {4488,coin,12000};
get_data(23) -> {4688,coin,13000};
get_data(24) -> {4888,coin,14000};
get_data(25) -> {5088,coin,15000};
get_data(26) -> {5288,coin,16000};
get_data(27) -> {5488,coin,17000};
get_data(28) -> {5688,coin,18000};
get_data(29) -> {5888,coin,19000};
get_data(30) -> {6088,coin,20000}.

