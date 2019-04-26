%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 每日任务
-module(mission_daily_setting).
-export([
		get_data/1
		,get_level/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95].


%% 获取指定类型的id列表
get_level(1) -> [1,2,3,7,8,9,13,14,15,16,20,21,22,23,27,28,29,30,34,42];
get_level(2) -> [4,5,6,10,11,12,17,18,19,24,25,26,31,32,33,35,36,37,38,43,44];
get_level(3) -> [39,40,41,45,46,74,75,76,80,81,86,87];
get_level(4) -> [47,48,49,50,65,66,67,77,78,82,83,84,88,89,90,92];
get_level(5) -> [51,52,56,68,79,85,91,93];
get_level(6) -> [53,54,57,58,59,62,69,70,71,94];
get_level(7) -> [55,60,61,63,64,72,73,95].


%% 根据id获取一条数据
%% 1 任务id
%% 2 任务等级
%% 3 动物
%% 4 奖励列表
get_data(1) -> {1,{turtle,10},[{coin,128}]};
get_data(2) -> {1,{turtle,12},[{locking,1}]};
get_data(3) -> {1,{turtle,14},[{coin,666}]};
get_data(4) -> {2,{turtle,16},[{locking,2}]};
get_data(5) -> {2,{turtle,18},[{coin,888}]};
get_data(6) -> {2,{turtle,20},[{locking,2}]};
get_data(7) -> {1,{cock,15},[{ice,1}]};
get_data(8) -> {1,{cock,16},[{coin,222}]};
get_data(9) -> {1,{cock,17},[{ice,1}]};
get_data(10) -> {2,{cock,18},[{coin,333}]};
get_data(11) -> {2,{cock,19},[{ice,2}]};
get_data(12) -> {2,{cock,20},[{coin,666}]};
get_data(13) -> {1,{dog,8},[{locking,1}]};
get_data(14) -> {1,{dog,10},[{coin,300}]};
get_data(15) -> {1,{dog,12},[{locking,1}]};
get_data(16) -> {1,{dog,14},[{coin,500}]};
get_data(17) -> {2,{dog,16},[{locking,2}]};
get_data(18) -> {2,{dog,18},[{coin,600}]};
get_data(19) -> {2,{dog,20},[{locking,2}]};
get_data(20) -> {1,{monkey,8},[{coin,168}]};
get_data(21) -> {1,{monkey,10},[{locking,1}]};
get_data(22) -> {1,{monkey,11},[{coin,256}]};
get_data(23) -> {1,{monkey,12},[{locking,1}]};
get_data(24) -> {2,{monkey,13},[{coin,512}]};
get_data(25) -> {2,{monkey,14},[{trumpet,1}]};
get_data(26) -> {2,{monkey,15},[{coin,800}]};
get_data(27) -> {1,{horse,8},[{ice,1}]};
get_data(28) -> {1,{horse,10},[{coin,199}]};
get_data(29) -> {1,{horse,11},[{ice,1}]};
get_data(30) -> {1,{horse,12},[{coin,299}]};
get_data(31) -> {2,{horse,13},[{auto,1}]};
get_data(32) -> {2,{horse,14},[{trumpet,1}]};
get_data(33) -> {2,{horse,15},[{rage,1}]};
get_data(34) -> {1,{ox,6},[{gold,10}]};
get_data(35) -> {2,{ox,8},[{gold,15}]};
get_data(36) -> {2,{ox,10},[{gold,20}]};
get_data(37) -> {2,{ox,11},[{gold,25}]};
get_data(38) -> {2,{ox,12},[{gold,30}]};
get_data(39) -> {3,{ox,13},[{gold,35}]};
get_data(40) -> {3,{ox,14},[{gold,40}]};
get_data(41) -> {3,{ox,15},[{gold,45}]};
get_data(42) -> {1,{panda,3},[{gold,10}]};
get_data(43) -> {2,{panda,4},[{gold,15}]};
get_data(44) -> {2,{panda,5},[{gold,20}]};
get_data(45) -> {3,{panda,6},[{gold,25}]};
get_data(46) -> {3,{panda,7},[{gold,30}]};
get_data(47) -> {4,{panda,8},[{gold,35}]};
get_data(48) -> {4,{panda,9},[{gold,40}]};
get_data(49) -> {4,{panda,10},[{horn,2}]};
get_data(50) -> {4,{hippo,5},[{tel_fare,30}]};
get_data(51) -> {5,{hippo,6},[{tel_fare,50}]};
get_data(52) -> {5,{hippo,7},[{tel_fare,60}]};
get_data(53) -> {6,{hippo,8},[{tel_fare,80}]};
get_data(54) -> {6,{hippo,9},[{tel_fare,100}]};
get_data(55) -> {7,{hippo,10},[{tel_fare,200}]};
get_data(56) -> {5,{lion,5},[{gold,50}]};
get_data(57) -> {6,{lion,6},[{gold,100}]};
get_data(58) -> {6,{lion,7},[{gold,150}]};
get_data(59) -> {6,{lion,8},[{gold,200}]};
get_data(60) -> {7,{lion,9},[{gold,250}]};
get_data(61) -> {7,{lion,10},[{gold,300}]};
get_data(62) -> {6,{elephant,1},[{red_bag,100}]};
get_data(63) -> {7,{elephant,2},[{red_bag,200}]};
get_data(64) -> {7,{elephant,3},[{red_bag,300}]};
get_data(65) -> {4,{pikachu,3},[{horn,1}]};
get_data(66) -> {4,{pikachu,4},[{rage,1}]};
get_data(67) -> {4,{pikachu,5},[{trumpet,2}]};
get_data(68) -> {5,{pikachu,6},[{auto,2}]};
get_data(69) -> {6,{pikachu,8},[{auto,2}]};
get_data(70) -> {6,{pikachu,10},[{auto,2}]};
get_data(71) -> {6,{bomber,1},[{red_bag,50}]};
get_data(72) -> {7,{bomber,2},[{red_bag,100}]};
get_data(73) -> {7,{bomber,3},[{red_bag,150}]};
get_data(74) -> {3,{type_bomber,3},[{gold,10}]};
get_data(75) -> {3,{type_bomber,4},[{gold,15}]};
get_data(76) -> {3,{type_bomber,5},[{gold,20}]};
get_data(77) -> {4,{type_bomber,6},[{gold,25}]};
get_data(78) -> {4,{type_bomber,8},[{gold,30}]};
get_data(79) -> {5,{type_bomber,10},[{gold,35}]};
get_data(80) -> {3,{xsx,3},[{horn,1}]};
get_data(81) -> {3,{xsx,4},[{rage,1}]};
get_data(82) -> {4,{xsx,5},[{trumpet,2}]};
get_data(83) -> {4,{xsx,6},[{auto,2}]};
get_data(84) -> {4,{xsx,8},[{auto,2}]};
get_data(85) -> {5,{xsx,10},[{auto,2}]};
get_data(86) -> {3,{dsy,3},[{coin,1000}]};
get_data(87) -> {3,{dsy,4},[{auto,1}]};
get_data(88) -> {4,{dsy,5},[{coin,2000}]};
get_data(89) -> {4,{dsy,6},[{auto,2}]};
get_data(90) -> {4,{dsy,8},[{coin,3000}]};
get_data(91) -> {5,{dsy,10},[{gold,35}]};
get_data(92) -> {4,{area_bomber,2},[{coin,2000}]};
get_data(93) -> {5,{area_bomber,3},[{coin,4000}]};
get_data(94) -> {6,{area_bomber,4},[{coin,6000}]};
get_data(95) -> {7,{area_bomber,5},[{coin,8000}]}.

