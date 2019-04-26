%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 每日任务-单人场
-module(mission_daily_6_setting).
-export([
		get_data/1
		,get_level/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [6000,6001,6002,6003,6004,6005,6006,6007,6008,6009,6010,6011,6012,6013,6014,6015,6016,6017,6018,6019,6020,6021,6022,6023,6024,6025,6026,6027,6028,6029,6030,6031,6032,6033,6034,6035,6036,6037,6038,6039,6040,6041,6042,6043,6044,6045,6046,6047,6048,6049,6050,6051,6052,6053,6054,6055,6056,6057,6058,6059,6060,6061,6062,6063,6064,6065,6066,6067,6068,6069,6070,6071,6072,6073,6074,6075,6076,6077,6078,6079,6080,6081,6082,6083,6084,6085,6086,6087,6088,6089,6090,6091,6092,6093,6094].


%% 获取指定类型的id列表
get_level(1) -> [6000,6001,6002,6006,6007,6008,6012,6013,6014,6015,6019,6020,6021,6022,6026,6027,6028,6029,6033,6041];
get_level(2) -> [6003,6004,6005,6009,6010,6011,6016,6017,6018,6023,6024,6025,6030,6031,6032,6034,6035,6036,6037,6042,6043];
get_level(3) -> [6038,6039,6040,6044,6045,6073,6074,6075,6079,6080,6085,6086];
get_level(4) -> [6046,6047,6048,6049,6064,6065,6066,6076,6077,6081,6082,6083,6087,6088,6089,6091];
get_level(5) -> [6050,6051,6055,6067,6078,6084,6090,6092];
get_level(6) -> [6052,6053,6056,6057,6058,6061,6068,6069,6070,6093];
get_level(7) -> [6054,6059,6060,6062,6063,6071,6072,6094].


%% 根据id获取一条数据
%% 1 任务id
%% 2 任务等级
%% 3 动物
%% 4 奖励列表
get_data(6000) -> {1,{turtle,10},[{coin,8}]};
get_data(6001) -> {1,{turtle,12},[{locking,1}]};
get_data(6002) -> {1,{turtle,14},[{coin,18}]};
get_data(6003) -> {2,{turtle,16},[{locking,1}]};
get_data(6004) -> {2,{turtle,18},[{coin,20}]};
get_data(6005) -> {2,{turtle,20},[{locking,1}]};
get_data(6006) -> {1,{cock,15},[{ice,1}]};
get_data(6007) -> {1,{cock,16},[{coin,30}]};
get_data(6008) -> {1,{cock,17},[{ice,1}]};
get_data(6009) -> {2,{cock,18},[{coin,60}]};
get_data(6010) -> {2,{cock,19},[{ice,1}]};
get_data(6011) -> {2,{cock,20},[{coin,88}]};
get_data(6012) -> {1,{dog,8},[{locking,1}]};
get_data(6013) -> {1,{dog,10},[{coin,30}]};
get_data(6014) -> {1,{dog,12},[{locking,1}]};
get_data(6015) -> {1,{dog,14},[{coin,40}]};
get_data(6016) -> {2,{dog,16},[{locking,1}]};
get_data(6017) -> {2,{dog,18},[{coin,50}]};
get_data(6018) -> {2,{dog,20},[{locking,1}]};
get_data(6019) -> {1,{monkey,8},[{coin,30}]};
get_data(6020) -> {1,{monkey,10},[{locking,1}]};
get_data(6021) -> {1,{monkey,11},[{coin,48}]};
get_data(6022) -> {1,{monkey,12},[{locking,1}]};
get_data(6023) -> {2,{monkey,13},[{coin,50}]};
get_data(6024) -> {2,{monkey,14},[{trumpet,1}]};
get_data(6025) -> {2,{monkey,15},[{coin,60}]};
get_data(6026) -> {1,{horse,8},[{ice,1}]};
get_data(6027) -> {1,{horse,10},[{coin,80}]};
get_data(6028) -> {1,{horse,11},[{ice,1}]};
get_data(6029) -> {1,{horse,12},[{coin,90}]};
get_data(6030) -> {2,{horse,13},[{auto,1}]};
get_data(6031) -> {2,{horse,14},[{trumpet,1}]};
get_data(6032) -> {2,{horse,15},[{rage,1}]};
get_data(6033) -> {1,{ox,6},[{coin,30}]};
get_data(6034) -> {2,{ox,8},[{coin,32}]};
get_data(6035) -> {2,{ox,10},[{coin,35}]};
get_data(6036) -> {2,{ox,11},[{coin,38}]};
get_data(6037) -> {2,{ox,12},[{coin,40}]};
get_data(6038) -> {3,{ox,13},[{coin,42}]};
get_data(6039) -> {3,{ox,14},[{coin,45}]};
get_data(6040) -> {3,{ox,15},[{coin,50}]};
get_data(6041) -> {1,{panda,3},[{coin,30}]};
get_data(6042) -> {2,{panda,4},[{coin,32}]};
get_data(6043) -> {2,{panda,5},[{coin,35}]};
get_data(6044) -> {3,{panda,6},[{coin,38}]};
get_data(6045) -> {3,{panda,7},[{coin,40}]};
get_data(6046) -> {4,{panda,8},[{coin,42}]};
get_data(6047) -> {4,{panda,9},[{coin,45}]};
get_data(6048) -> {4,{panda,10},[{coin,50}]};
get_data(6049) -> {4,{hippo,5},[{coin,125}]};
get_data(6050) -> {5,{hippo,6},[{coin,150}]};
get_data(6051) -> {5,{hippo,7},[{coin,175}]};
get_data(6052) -> {6,{hippo,8},[{coin,200}]};
get_data(6053) -> {6,{hippo,9},[{coin,225}]};
get_data(6054) -> {7,{hippo,10},[{coin,250}]};
get_data(6055) -> {5,{lion,5},[{gold,2}]};
get_data(6056) -> {6,{lion,6},[{gold,3}]};
get_data(6057) -> {6,{lion,7},[{gold,3}]};
get_data(6058) -> {6,{lion,8},[{gold,4}]};
get_data(6059) -> {7,{lion,9},[{gold,4}]};
get_data(6060) -> {7,{lion,10},[{gold,5}]};
get_data(6061) -> {6,{elephant,1},[{coin,250}]};
get_data(6062) -> {7,{elephant,2},[{coin,500}]};
get_data(6063) -> {7,{elephant,3},[{coin,750}]};
get_data(6064) -> {4,{pikachu,3},[{horn,1}]};
get_data(6065) -> {4,{pikachu,4},[{rage,1}]};
get_data(6066) -> {4,{pikachu,5},[{trumpet,1}]};
get_data(6067) -> {5,{pikachu,6},[{auto,1}]};
get_data(6068) -> {6,{pikachu,8},[{auto,1}]};
get_data(6069) -> {6,{pikachu,10},[{auto,1}]};
get_data(6070) -> {6,{bomber,1},[{coin,30}]};
get_data(6071) -> {7,{bomber,2},[{coin,40}]};
get_data(6072) -> {7,{bomber,3},[{coin,50}]};
get_data(6073) -> {3,{type_bomber,3},[{gold,1}]};
get_data(6074) -> {3,{type_bomber,4},[{gold,2}]};
get_data(6075) -> {3,{type_bomber,5},[{gold,3}]};
get_data(6076) -> {4,{type_bomber,6},[{gold,4}]};
get_data(6077) -> {4,{type_bomber,8},[{gold,5}]};
get_data(6078) -> {5,{type_bomber,10},[{gold,6}]};
get_data(6079) -> {3,{xsx,3},[{horn,1}]};
get_data(6080) -> {3,{xsx,4},[{rage,1}]};
get_data(6081) -> {4,{xsx,5},[{trumpet,1}]};
get_data(6082) -> {4,{xsx,6},[{auto,1}]};
get_data(6083) -> {4,{xsx,8},[{auto,1}]};
get_data(6084) -> {5,{xsx,10},[{auto,1}]};
get_data(6085) -> {3,{dsy,3},[{coin,45}]};
get_data(6086) -> {3,{dsy,4},[{auto,1}]};
get_data(6087) -> {4,{dsy,5},[{coin,75}]};
get_data(6088) -> {4,{dsy,6},[{auto,1}]};
get_data(6089) -> {4,{dsy,8},[{coin,120}]};
get_data(6090) -> {5,{dsy,10},[{gold,2}]};
get_data(6091) -> {4,{area_bomber,2},[{coin,118}]};
get_data(6092) -> {5,{area_bomber,3},[{coin,128}]};
get_data(6093) -> {6,{area_bomber,4},[{coin,158}]};
get_data(6094) -> {7,{area_bomber,5},[{coin,168}]}.

