%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 每日任务-小资场
-module(mission_daily_1_setting).
-export([
		get_data/1
		,get_level/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035,2036,2037,2038,2039,2040,2041,2042,2043,2044,2045,2046,2047,2048,2049,2050,2051,2052,2053,2054,2055,2056,2057,2058,2059,2060,2061,2062,2063,2064,2065,2066,2067,2068,2069,2070,2071,2072,2073,2074,2075,2076,2077,2078,2079,2080,2081,2082,2083,2084,2085,2086,2087,2088,2089,2090,2091,2092,2093,2094].


%% 获取指定类型的id列表
get_level(1) -> [2000,2001,2002,2006,2007,2008,2012,2013,2014,2015,2019,2020,2021,2022,2026,2027,2028,2029,2033,2041];
get_level(2) -> [2003,2004,2005,2009,2010,2011,2016,2017,2018,2023,2024,2025,2030,2031,2032,2034,2035,2036,2037,2042,2043];
get_level(3) -> [2038,2039,2040,2044,2045,2073,2074,2075,2079,2080,2085,2086];
get_level(4) -> [2046,2047,2048,2049,2064,2065,2066,2076,2077,2081,2082,2083,2087,2088,2089,2091];
get_level(5) -> [2050,2051,2055,2067,2078,2084,2090,2092];
get_level(6) -> [2052,2053,2056,2057,2058,2061,2068,2069,2070,2093];
get_level(7) -> [2054,2059,2060,2062,2063,2071,2072,2094].


%% 根据id获取一条数据
%% 1 任务id
%% 2 任务等级
%% 3 动物
%% 4 奖励列表
get_data(2000) -> {1,{turtle,10},[{coin,48}]};
get_data(2001) -> {1,{turtle,12},[{locking,1}]};
get_data(2002) -> {1,{turtle,14},[{coin,68}]};
get_data(2003) -> {2,{turtle,16},[{locking,1}]};
get_data(2004) -> {2,{turtle,18},[{coin,88}]};
get_data(2005) -> {2,{turtle,20},[{locking,1}]};
get_data(2006) -> {1,{cock,15},[{ice,1}]};
get_data(2007) -> {1,{cock,16},[{coin,95}]};
get_data(2008) -> {1,{cock,17},[{ice,1}]};
get_data(2009) -> {2,{cock,18},[{coin,100}]};
get_data(2010) -> {2,{cock,19},[{ice,1}]};
get_data(2011) -> {2,{cock,20},[{coin,108}]};
get_data(2012) -> {1,{dog,8},[{locking,1}]};
get_data(2013) -> {1,{dog,10},[{coin,98}]};
get_data(2014) -> {1,{dog,12},[{locking,1}]};
get_data(2015) -> {1,{dog,14},[{coin,138}]};
get_data(2016) -> {2,{dog,16},[{locking,1}]};
get_data(2017) -> {2,{dog,18},[{coin,180}]};
get_data(2018) -> {2,{dog,20},[{locking,1}]};
get_data(2019) -> {1,{monkey,8},[{coin,158}]};
get_data(2020) -> {1,{monkey,10},[{locking,1}]};
get_data(2021) -> {1,{monkey,11},[{coin,210}]};
get_data(2022) -> {1,{monkey,12},[{locking,1}]};
get_data(2023) -> {2,{monkey,13},[{coin,250}]};
get_data(2024) -> {2,{monkey,14},[{trumpet,1}]};
get_data(2025) -> {2,{monkey,15},[{coin,300}]};
get_data(2026) -> {1,{horse,8},[{ice,1}]};
get_data(2027) -> {1,{horse,10},[{coin,280}]};
get_data(2028) -> {1,{horse,11},[{ice,1}]};
get_data(2029) -> {1,{horse,12},[{coin,350}]};
get_data(2030) -> {2,{horse,13},[{auto,1}]};
get_data(2031) -> {2,{horse,14},[{trumpet,1}]};
get_data(2032) -> {2,{horse,15},[{rage,1}]};
get_data(2033) -> {1,{ox,6},[{gold,1}]};
get_data(2034) -> {2,{ox,8},[{gold,1}]};
get_data(2035) -> {2,{ox,10},[{gold,2}]};
get_data(2036) -> {2,{ox,11},[{gold,2}]};
get_data(2037) -> {2,{ox,12},[{gold,3}]};
get_data(2038) -> {3,{ox,13},[{gold,3}]};
get_data(2039) -> {3,{ox,14},[{gold,4}]};
get_data(2040) -> {3,{ox,15},[{gold,4}]};
get_data(2041) -> {1,{panda,3},[{gold,2}]};
get_data(2042) -> {2,{panda,4},[{gold,3}]};
get_data(2043) -> {2,{panda,5},[{gold,4}]};
get_data(2044) -> {3,{panda,6},[{gold,5}]};
get_data(2045) -> {3,{panda,7},[{gold,6}]};
get_data(2046) -> {4,{panda,8},[{gold,7}]};
get_data(2047) -> {4,{panda,9},[{gold,8}]};
get_data(2048) -> {4,{panda,10},[{horn,1}]};
get_data(2049) -> {4,{hippo,5},[{tel_fare,2}]};
get_data(2050) -> {5,{hippo,6},[{tel_fare,3}]};
get_data(2051) -> {5,{hippo,7},[{tel_fare,3}]};
get_data(2052) -> {6,{hippo,8},[{tel_fare,4}]};
get_data(2053) -> {6,{hippo,9},[{tel_fare,4}]};
get_data(2054) -> {7,{hippo,10},[{tel_fare,5}]};
get_data(2055) -> {5,{lion,5},[{gold,5}]};
get_data(2056) -> {6,{lion,6},[{gold,6}]};
get_data(2057) -> {6,{lion,7},[{gold,7}]};
get_data(2058) -> {6,{lion,8},[{gold,8}]};
get_data(2059) -> {7,{lion,9},[{gold,9}]};
get_data(2060) -> {7,{lion,10},[{gold,10}]};
get_data(2061) -> {6,{elephant,1},[{red_bag,5}]};
get_data(2062) -> {7,{elephant,2},[{red_bag,10}]};
get_data(2063) -> {7,{elephant,3},[{red_bag,15}]};
get_data(2064) -> {4,{pikachu,3},[{horn,1}]};
get_data(2065) -> {4,{pikachu,4},[{rage,1}]};
get_data(2066) -> {4,{pikachu,5},[{trumpet,1}]};
get_data(2067) -> {5,{pikachu,6},[{auto,1}]};
get_data(2068) -> {6,{pikachu,8},[{auto,1}]};
get_data(2069) -> {6,{pikachu,10},[{auto,1}]};
get_data(2070) -> {6,{bomber,1},[{red_bag,5}]};
get_data(2071) -> {7,{bomber,2},[{red_bag,10}]};
get_data(2072) -> {7,{bomber,3},[{red_bag,15}]};
get_data(2073) -> {3,{type_bomber,3},[{gold,1}]};
get_data(2074) -> {3,{type_bomber,4},[{gold,2}]};
get_data(2075) -> {3,{type_bomber,5},[{gold,3}]};
get_data(2076) -> {4,{type_bomber,6},[{gold,4}]};
get_data(2077) -> {4,{type_bomber,8},[{gold,5}]};
get_data(2078) -> {5,{type_bomber,10},[{gold,6}]};
get_data(2079) -> {3,{xsx,3},[{horn,1}]};
get_data(2080) -> {3,{xsx,4},[{rage,1}]};
get_data(2081) -> {4,{xsx,5},[{trumpet,1}]};
get_data(2082) -> {4,{xsx,6},[{auto,1}]};
get_data(2083) -> {4,{xsx,8},[{auto,1}]};
get_data(2084) -> {5,{xsx,10},[{auto,1}]};
get_data(2085) -> {3,{dsy,3},[{coin,888}]};
get_data(2086) -> {3,{dsy,4},[{auto,1}]};
get_data(2087) -> {4,{dsy,5},[{coin,1480}]};
get_data(2088) -> {4,{dsy,6},[{auto,1}]};
get_data(2089) -> {4,{dsy,8},[{coin,2400}]};
get_data(2090) -> {5,{dsy,10},[{gold,30}]};
get_data(2091) -> {4,{area_bomber,2},[{coin,1000}]};
get_data(2092) -> {5,{area_bomber,3},[{coin,1200}]};
get_data(2093) -> {6,{area_bomber,4},[{coin,1400}]};
get_data(2094) -> {7,{area_bomber,5},[{coin,1600}]}.

