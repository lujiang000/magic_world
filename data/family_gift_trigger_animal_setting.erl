%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 联盟礼物抓捕动物触发器
-module(family_gift_trigger_animal_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5,6,7,8,9,10].


%% 根据id获取一条数据
%% 1 等级
%% 2 升级所需经验
%% 3 礼物宝箱ID(FamilyGiftRewardSetting)
get_data(1) -> {[0,10],200};
get_data(2) -> {[0,10],201};
get_data(3) -> {[0,10],202};
get_data(4) -> {[0,10],203};
get_data(5) -> {[0,10],204};
get_data(6) -> {[0,10],200};
get_data(7) -> {[0,10],201};
get_data(8) -> {[0,10],202};
get_data(9) -> {[0,10],203};
get_data(10) -> {[0,10],204}.

