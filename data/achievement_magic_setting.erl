%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 解锁魔法
-module(achievement_magic_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28].


%% 根据id获取一条数据
%% 1 id
%% 2 魔法档位
%% 3 道具类型(server)
%% 4 道具数量
get_data(1) -> {2,gold,1};
get_data(2) -> {4,gold,2};
get_data(3) -> {6,gold,3};
get_data(4) -> {11,gold,4};
get_data(5) -> {13,gold,5};
get_data(6) -> {15,gold,6};
get_data(7) -> {20,gold,7};
get_data(8) -> {21,gold,8};
get_data(9) -> {23,gold,9};
get_data(10) -> {25,gold,10};
get_data(11) -> {27,gold,11};
get_data(12) -> {29,gold,12};
get_data(13) -> {31,gold,13};
get_data(14) -> {33,gold,14};
get_data(15) -> {35,gold,15};
get_data(16) -> {36,gold,16};
get_data(17) -> {41,gold,17};
get_data(18) -> {46,gold,18};
get_data(19) -> {47,gold,19};
get_data(20) -> {48,gold,20};
get_data(21) -> {49,gold,21};
get_data(22) -> {50,gold,22};
get_data(23) -> {51,gold,23};
get_data(24) -> {52,gold,24};
get_data(25) -> {53,gold,25};
get_data(26) -> {54,gold,26};
get_data(27) -> {55,gold,27};
get_data(28) -> {56,gold,28}.

