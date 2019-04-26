%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 家族联盟活动任务
-module(family_activity_mission_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47].


%% 根据id获取一条数据
%% 1 任务id
%% 2 动物类型和数量
%% 3 奖励积分
get_data(1) -> {{turtle,3},1};
get_data(2) -> {{turtle,4},2};
get_data(3) -> {{turtle,5},3};
get_data(4) -> {{turtle,6},4};
get_data(5) -> {{turtle,7},5};
get_data(6) -> {{turtle,8},6};
get_data(7) -> {{cock,3},7};
get_data(8) -> {{cock,4},8};
get_data(9) -> {{cock,5},9};
get_data(10) -> {{cock,6},10};
get_data(11) -> {{cock,7},11};
get_data(12) -> {{cock,8},12};
get_data(13) -> {{dog,3},13};
get_data(14) -> {{dog,4},14};
get_data(15) -> {{dog,5},15};
get_data(16) -> {{dog,6},16};
get_data(17) -> {{dog,7},17};
get_data(18) -> {{dog,8},18};
get_data(19) -> {{monkey,3},19};
get_data(20) -> {{monkey,4},20};
get_data(21) -> {{monkey,5},21};
get_data(22) -> {{monkey,6},22};
get_data(23) -> {{monkey,7},23};
get_data(24) -> {{monkey,8},24};
get_data(25) -> {{horse,3},25};
get_data(26) -> {{horse,4},26};
get_data(27) -> {{horse,5},27};
get_data(28) -> {{horse,6},28};
get_data(29) -> {{ox,3},29};
get_data(30) -> {{ox,4},30};
get_data(31) -> {{ox,5},31};
get_data(32) -> {{ox,6},32};
get_data(33) -> {{panda,2},33};
get_data(34) -> {{panda,3},34};
get_data(35) -> {{panda,4},35};
get_data(36) -> {{panda,5},36};
get_data(37) -> {{pikachu,1},37};
get_data(38) -> {{pikachu,2},38};
get_data(39) -> {{type_bomber,1},39};
get_data(40) -> {{type_bomber,2},40};
get_data(41) -> {{type_bomber,3},41};
get_data(42) -> {{type_bomber,4},42};
get_data(43) -> {{type_bomber,5},43};
get_data(44) -> {{xsx,1},44};
get_data(45) -> {{dsy,1},45};
get_data(46) -> {{hippo,1},46};
get_data(47) -> {{lion,1},47}.

