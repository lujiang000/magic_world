%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 登陆累计次数
-module(achievement_login_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30].


%% 根据id获取一条数据
%% 1 id
%% 2 累计登陆次数
%% 3 道具类型(server)
%% 4 道具数量
get_data(1) -> {1,coin,800};
get_data(2) -> {2,coin,1000};
get_data(3) -> {3,coin,1100};
get_data(4) -> {4,coin,1200};
get_data(5) -> {5,coin,1300};
get_data(6) -> {6,coin,1400};
get_data(7) -> {7,coin,1500};
get_data(8) -> {8,coin,1600};
get_data(9) -> {9,coin,1700};
get_data(10) -> {10,coin,1800};
get_data(11) -> {11,coin,1900};
get_data(12) -> {12,coin,2000};
get_data(13) -> {13,coin,2200};
get_data(14) -> {14,coin,2400};
get_data(15) -> {15,coin,2600};
get_data(16) -> {16,coin,2800};
get_data(17) -> {17,coin,3000};
get_data(18) -> {18,coin,3500};
get_data(19) -> {19,coin,4000};
get_data(20) -> {20,coin,4500};
get_data(21) -> {21,coin,5000};
get_data(22) -> {22,coin,5500};
get_data(23) -> {23,coin,6000};
get_data(24) -> {24,coin,8000};
get_data(25) -> {25,coin,8500};
get_data(26) -> {26,coin,9000};
get_data(27) -> {27,coin,9500};
get_data(28) -> {28,coin,10000};
get_data(29) -> {29,coin,12000};
get_data(30) -> {30,coin,15000}.

