%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
-module(zoo_new_player_task_setting).
-export([
		get_data/1
	]).

%% 根据id获取一条数据
%% 1 任务ID
%% 2 奖励列表
%% 3 
%% 4 数量
get_data(1) -> {[{gold,2}],{animal_die, cock},5};
get_data(2) -> {[{ice,2}],{open_fire, 3},1};
get_data(3) -> {[{coin,1000}],{use_skill, ice},1};
get_data(4) -> {[{gold,8}],{animal_die, all},8};
get_data(5) -> {[{coin,8000}],{open_fire, 6},1}.

