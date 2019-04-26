%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 每日任务
-module(mission_sign_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7].


%% 根据id获取一条数据
%% 1 任务id
%% 2 奖励列表
get_data(1) -> {[{coin,1000}]};
get_data(2) -> {[{coin,2000}]};
get_data(3) -> {[{coin,3000}]};
get_data(4) -> {[{coin,4000}]};
get_data(5) -> {[{coin,5000}]};
get_data(6) -> {[{coin,8000}]};
get_data(7) -> {[{coin,10000}]}.

