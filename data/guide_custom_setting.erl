%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 常规指引
-module(guide_custom_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5].


%% 根据id获取一条数据
%% 1 任务ID
%% 2 奖励列表
%% 3 
%% 4 数量
get_data(1) -> {[{coin,100}],{animal_die, dog},1};
get_data(2) -> {[{coin,2000}],{animal_die, all},5};
get_data(3) -> {[{coin,3000}],{animal_die, hippo},1};
get_data(4) -> {[{gold,10}],{animal_die, hippo},5};
get_data(5) -> {[{coin,3000}],{red_bag, 40},40}.

