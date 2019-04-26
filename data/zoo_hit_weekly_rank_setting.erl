%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 动物园击杀榜
-module(zoo_hit_weekly_rank_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20].


%% 根据id获取一条数据
%% 1 名次
%% 2 奖励列表
get_data(1) -> {[{lollipop,4}]};
get_data(2) -> {[{lollipop,2}]};
get_data(3) -> {[{lollipop,1}]};
get_data(4) -> {[{lolly,4}]};
get_data(5) -> {[{lolly,3}]};
get_data(6) -> {[{lolly,2}]};
get_data(7) -> {[{lolly,1}]};
get_data(8) -> {[{candy,8}]};
get_data(9) -> {[{candy,6}]};
get_data(10) -> {[{candy,4}]};
get_data(11) -> {[{candy,2}]};
get_data(12) -> {[{candy,2}]};
get_data(13) -> {[{candy,2}]};
get_data(14) -> {[{candy,2}]};
get_data(15) -> {[{candy,2}]};
get_data(16) -> {[{candy,2}]};
get_data(17) -> {[{candy,2}]};
get_data(18) -> {[{candy,2}]};
get_data(19) -> {[{candy,2}]};
get_data(20) -> {[{candy,2}]}.

