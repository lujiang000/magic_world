%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 限时任务
-module(mission_time_limit_setting).
-export([
		get_data/1
		,get_level/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46].


%% 获取指定类型的id列表
get_level(1) -> [1,2,3,4,5,6,7,9,10,11,12,13,14,15,17,18,19,20,21,23,24,25,26,27,29,30,33,34];
get_level(2) -> [8,16,22,28,31,32,35,36,37,38,44];
get_level(3) -> [39,40,41,42,43,45,46].


%% 根据id获取一条数据
%% 1 任务id
%% 2 任务等级
%% 3 动物类型(server)
%% 4 动物数量
get_data(1) -> {1,turtle,3};
get_data(2) -> {1,turtle,4};
get_data(3) -> {1,turtle,5};
get_data(4) -> {1,turtle,6};
get_data(5) -> {1,turtle,7};
get_data(6) -> {1,turtle,8};
get_data(7) -> {1,turtle,9};
get_data(8) -> {2,turtle,10};
get_data(9) -> {1,cock,3};
get_data(10) -> {1,cock,4};
get_data(11) -> {1,cock,5};
get_data(12) -> {1,cock,6};
get_data(13) -> {1,cock,7};
get_data(14) -> {1,cock,8};
get_data(15) -> {1,cock,9};
get_data(16) -> {2,cock,10};
get_data(17) -> {1,dog,3};
get_data(18) -> {1,dog,4};
get_data(19) -> {1,dog,5};
get_data(20) -> {1,dog,6};
get_data(21) -> {1,dog,7};
get_data(22) -> {2,dog,8};
get_data(23) -> {1,monkey,3};
get_data(24) -> {1,monkey,4};
get_data(25) -> {1,monkey,5};
get_data(26) -> {1,monkey,6};
get_data(27) -> {1,monkey,7};
get_data(28) -> {2,monkey,8};
get_data(29) -> {1,horse,3};
get_data(30) -> {1,horse,4};
get_data(31) -> {2,horse,5};
get_data(32) -> {2,horse,6};
get_data(33) -> {1,ox,3};
get_data(34) -> {1,ox,4};
get_data(35) -> {2,ox,5};
get_data(36) -> {2,ox,6};
get_data(37) -> {2,panda,2};
get_data(38) -> {2,panda,3};
get_data(39) -> {3,panda,4};
get_data(40) -> {3,hippo,1};
get_data(41) -> {3,lion,1};
get_data(42) -> {3,pikachu,1};
get_data(43) -> {3,type_bomber,1};
get_data(44) -> {2,xsx,1};
get_data(45) -> {3,dsy,1};
get_data(46) -> {3,area_bomber,1}.

