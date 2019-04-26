%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 每日任务-钻石场
-module(mission_daily_3_setting).
-export([
		get_data/1
		,get_level/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [5095,5096,5097,5098,5099,5100,5101,5102,5103,5104,5105,5106,5107,5108,5109,5110,5111,5112,5113,5114,5115,5116,5117,5118,5119,5120,5121,5122,5123,5124,5125,5126,5127,5128,5129,5130,5131,5132,5133,5134,5135,5136,5137,5138,5139,5140,5141,5142,5143,5144,5145,5146,5147,5148,5149,5150,5151,5152,5153,5154,5155,5156,5157,5158,5159,5160,5161,5162,5163,5164,5165,5166,5167,5168,5169,5170].


%% 获取指定类型的id列表
get_level(1) -> [5095,5096,5097,5098,5102,5103,5104,5105,5109,5117];
get_level(2) -> [5099,5100,5101,5106,5107,5108,5110,5111,5112,5113,5118,5119];
get_level(3) -> [5114,5115,5116,5120,5121,5149,5150,5151,5155,5156,5161,5162];
get_level(4) -> [5122,5123,5124,5125,5140,5141,5142,5152,5153,5157,5158,5159,5163,5164,5165,5167];
get_level(5) -> [5126,5127,5131,5143,5154,5160,5166,5168];
get_level(6) -> [5128,5129,5132,5133,5134,5137,5144,5145,5146,5169];
get_level(7) -> [5130,5135,5136,5138,5139,5147,5148,5170].


%% 根据id获取一条数据
%% 1 任务id
%% 2 任务等级
%% 3 动物
%% 4 奖励列表
get_data(5095) -> {1,{monkey,4},[{coin,27800}]};
get_data(5096) -> {1,{monkey,5},[{locking,2}]};
get_data(5097) -> {1,{monkey,6},[{coin,36600}]};
get_data(5098) -> {1,{monkey,7},[{locking,3}]};
get_data(5099) -> {2,{monkey,8},[{coin,51200}]};
get_data(5100) -> {2,{monkey,9},[{trumpet,2}]};
get_data(5101) -> {2,{monkey,10},[{coin,60000}]};
get_data(5102) -> {1,{horse,4},[{ice,2}]};
get_data(5103) -> {1,{horse,5},[{coin,59000}]};
get_data(5104) -> {1,{horse,6},[{ice,2}]};
get_data(5105) -> {1,{horse,7},[{coin,69000}]};
get_data(5106) -> {2,{horse,8},[{auto,2}]};
get_data(5107) -> {2,{horse,9},[{trumpet,2}]};
get_data(5108) -> {2,{horse,10},[{rage,2}]};
get_data(5109) -> {1,{ox,3},[{gold,58}]};
get_data(5110) -> {2,{ox,4},[{gold,78}]};
get_data(5111) -> {2,{ox,5},[{gold,98}]};
get_data(5112) -> {2,{ox,6},[{gold,108}]};
get_data(5113) -> {2,{ox,7},[{gold,118}]};
get_data(5114) -> {3,{ox,8},[{gold,128}]};
get_data(5115) -> {3,{ox,9},[{gold,138}]};
get_data(5116) -> {3,{ox,10},[{gold,148}]};
get_data(5117) -> {1,{panda,3},[{gold,58}]};
get_data(5118) -> {2,{panda,4},[{gold,78}]};
get_data(5119) -> {2,{panda,5},[{gold,98}]};
get_data(5120) -> {3,{panda,6},[{gold,118}]};
get_data(5121) -> {3,{panda,7},[{gold,138}]};
get_data(5122) -> {4,{panda,8},[{gold,158}]};
get_data(5123) -> {4,{panda,9},[{gold,180}]};
get_data(5124) -> {4,{panda,10},[{horn,2}]};
get_data(5125) -> {4,{hippo,3},[{tel_fare,350}]};
get_data(5126) -> {5,{hippo,4},[{tel_fare,550}]};
get_data(5127) -> {5,{hippo,5},[{tel_fare,650}]};
get_data(5128) -> {6,{hippo,6},[{tel_fare,850}]};
get_data(5129) -> {6,{hippo,7},[{tel_fare,950}]};
get_data(5130) -> {7,{hippo,8},[{tel_fare,1100}]};
get_data(5131) -> {5,{lion,3},[{gold,800}]};
get_data(5132) -> {6,{lion,4},[{gold,1000}]};
get_data(5133) -> {6,{lion,5},[{gold,1300}]};
get_data(5134) -> {6,{lion,6},[{gold,1500}]};
get_data(5135) -> {7,{lion,7},[{gold,1700}]};
get_data(5136) -> {7,{lion,8},[{gold,2000}]};
get_data(5137) -> {6,{elephant,1},[{red_bag,1500}]};
get_data(5138) -> {7,{elephant,2},[{red_bag,2500}]};
get_data(5139) -> {7,{elephant,3},[{red_bag,3500}]};
get_data(5140) -> {4,{pikachu,3},[{horn,3}]};
get_data(5141) -> {4,{pikachu,4},[{rage,2}]};
get_data(5142) -> {4,{pikachu,5},[{trumpet,3}]};
get_data(5143) -> {5,{pikachu,6},[{auto,3}]};
get_data(5144) -> {6,{pikachu,7},[{auto,4}]};
get_data(5145) -> {6,{pikachu,8},[{auto,5}]};
get_data(5146) -> {6,{bomber,1},[{red_bag,600}]};
get_data(5147) -> {7,{bomber,2},[{red_bag,1500}]};
get_data(5148) -> {7,{bomber,3},[{red_bag,2000}]};
get_data(5149) -> {3,{type_bomber,3},[{gold,100}]};
get_data(5150) -> {3,{type_bomber,4},[{gold,150}]};
get_data(5151) -> {3,{type_bomber,5},[{gold,200}]};
get_data(5152) -> {4,{type_bomber,6},[{gold,250}]};
get_data(5153) -> {4,{type_bomber,7},[{gold,300}]};
get_data(5154) -> {5,{type_bomber,8},[{gold,350}]};
get_data(5155) -> {3,{xsx,3},[{horn,2}]};
get_data(5156) -> {3,{xsx,4},[{rage,2}]};
get_data(5157) -> {4,{xsx,5},[{trumpet,3}]};
get_data(5158) -> {4,{xsx,6},[{auto,3}]};
get_data(5159) -> {4,{xsx,7},[{auto,3}]};
get_data(5160) -> {5,{xsx,8},[{auto,3}]};
get_data(5161) -> {3,{dsy,3},[{coin,170000}]};
get_data(5162) -> {3,{dsy,4},[{auto,1}]};
get_data(5163) -> {4,{dsy,5},[{coin,300000}]};
get_data(5164) -> {4,{dsy,6},[{auto,2}]};
get_data(5165) -> {4,{dsy,7},[{coin,480000}]};
get_data(5166) -> {5,{dsy,8},[{gold,600}]};
get_data(5167) -> {4,{area_bomber,2},[{coin,20000}]};
get_data(5168) -> {5,{area_bomber,3},[{coin,40000}]};
get_data(5169) -> {6,{area_bomber,4},[{coin,60000}]};
get_data(5170) -> {7,{area_bomber,5},[{coin,80000}]}.

