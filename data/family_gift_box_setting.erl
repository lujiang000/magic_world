%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 联盟礼物宝箱
-module(family_gift_box_setting).
-export([
		get_data/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [1,2,3,4,5].


%% 根据id获取一条数据
%% 1 等级
%% 2 升级所需经验
%% 3 宝箱奖励ID(FamilyGiftRewardSetting)
get_data(1) -> {500,1};
get_data(2) -> {1000,2};
get_data(3) -> {10000,3};
get_data(4) -> {50000,4};
get_data(5) -> {100000,5}.

