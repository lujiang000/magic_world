%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 大奖赛日榜
-module(arena_rank_daily_reward_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50].


%% 根据id获取一条数据
%% 1 名次
%% 2 奖励列表
get_data(1) -> {[{lollipop,20}]};
get_data(2) -> {[{lollipop,10}]};
get_data(3) -> {[{lollipop,5}]};
get_data(4) -> {[{lollipop,3}]};
get_data(5) -> {[{lollipop,3}]};
get_data(6) -> {[{lollipop,3}]};
get_data(7) -> {[{lollipop,3}]};
get_data(8) -> {[{lollipop,3}]};
get_data(9) -> {[{lollipop,3}]};
get_data(10) -> {[{lollipop,3}]};
get_data(11) -> {[{lolly,3}]};
get_data(12) -> {[{lolly,3}]};
get_data(13) -> {[{lolly,3}]};
get_data(14) -> {[{lolly,3}]};
get_data(15) -> {[{lolly,3}]};
get_data(16) -> {[{lolly,3}]};
get_data(17) -> {[{lolly,3}]};
get_data(18) -> {[{lolly,3}]};
get_data(19) -> {[{lolly,3}]};
get_data(20) -> {[{lolly,3}]};
get_data(21) -> {[{lolly,3}]};
get_data(22) -> {[{lolly,3}]};
get_data(23) -> {[{lolly,3}]};
get_data(24) -> {[{lolly,3}]};
get_data(25) -> {[{lolly,3}]};
get_data(26) -> {[{lolly,3}]};
get_data(27) -> {[{lolly,3}]};
get_data(28) -> {[{lolly,3}]};
get_data(29) -> {[{lolly,3}]};
get_data(30) -> {[{lolly,3}]};
get_data(31) -> {[{candy,2}]};
get_data(32) -> {[{candy,2}]};
get_data(33) -> {[{candy,2}]};
get_data(34) -> {[{candy,2}]};
get_data(35) -> {[{candy,2}]};
get_data(36) -> {[{candy,2}]};
get_data(37) -> {[{candy,2}]};
get_data(38) -> {[{candy,2}]};
get_data(39) -> {[{candy,2}]};
get_data(40) -> {[{candy,2}]};
get_data(41) -> {[{candy,2}]};
get_data(42) -> {[{candy,2}]};
get_data(43) -> {[{candy,2}]};
get_data(44) -> {[{candy,2}]};
get_data(45) -> {[{candy,2}]};
get_data(46) -> {[{candy,2}]};
get_data(47) -> {[{candy,2}]};
get_data(48) -> {[{candy,2}]};
get_data(49) -> {[{candy,2}]};
get_data(50) -> {[{candy,2}]}.

