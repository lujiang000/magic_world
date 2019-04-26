%% 文件根据Excel配置表自动生成,修改可能会产生意外的问题,并且任何改动将在文件重新生成时丢失
%% 每日任务-富豪场
-module(mission_daily_2_setting).
-export([
		get_data/1
		,get_level/1
		,get_all/0
	]).

%% 获取所有id列表
get_all() -> [3095,3096,3097,3098,3099,3100,3101,3102,3103,3104,3105,3106,3107,3108,3109,3110,3111,3112,3113,3114,3115,3116,3117,3118,3119,3120,3121,3122,3123,3124,3125,3126,3127,3128,3129,3130,3131,3132,3133,3134,3135,3136,3137,3138,3139,3140,3141,3142,3143,3144,3145,3146,3147,3148,3149,3150,3151,3152,3153,3154,3155,3156,3157,3158,3159,3160,3161,3162,3163,3164,3165,3166,3167,3168,3169,3170,3171,3172,3173,3174,3175,3176,3177,3178,3179,3180,3181,3182,3183,3184,3185,3186,3187,3188,3189].


%% 获取指定类型的id列表
get_level(1) -> [3095,3096,3097,3101,3102,3103,3107,3108,3109,3110,3114,3115,3116,3117,3121,3122,3123,3124,3128,3136];
get_level(2) -> [3098,3099,3100,3104,3105,3106,3111,3112,3113,3118,3119,3120,3125,3126,3127,3129,3130,3131,3132,3137,3138];
get_level(3) -> [3133,3134,3135,3139,3140,3168,3169,3170,3174,3175,3180,3181];
get_level(4) -> [3141,3142,3143,3144,3159,3160,3161,3171,3172,3176,3177,3178,3182,3183,3184,3186];
get_level(5) -> [3145,3146,3150,3162,3173,3179,3185,3187];
get_level(6) -> [3147,3148,3151,3152,3153,3156,3163,3164,3165,3188];
get_level(7) -> [3149,3154,3155,3157,3158,3166,3167,3189].


%% 根据id获取一条数据
%% 1 任务id
%% 2 任务等级
%% 3 动物
%% 4 奖励列表
get_data(3095) -> {1,{turtle,5},[{coin,480}]};
get_data(3096) -> {1,{turtle,6},[{locking,1}]};
get_data(3097) -> {1,{turtle,7},[{coin,680}]};
get_data(3098) -> {2,{turtle,8},[{locking,2}]};
get_data(3099) -> {2,{turtle,9},[{coin,880}]};
get_data(3100) -> {2,{turtle,10},[{locking,2}]};
get_data(3101) -> {1,{cock,5},[{ice,1}]};
get_data(3102) -> {1,{cock,6},[{coin,950}]};
get_data(3103) -> {1,{cock,7},[{ice,1}]};
get_data(3104) -> {2,{cock,8},[{coin,1000}]};
get_data(3105) -> {2,{cock,9},[{ice,2}]};
get_data(3106) -> {2,{cock,10},[{coin,1080}]};
get_data(3107) -> {1,{dog,4},[{locking,1}]};
get_data(3108) -> {1,{dog,5},[{coin,980}]};
get_data(3109) -> {1,{dog,6},[{locking,1}]};
get_data(3110) -> {1,{dog,7},[{coin,1380}]};
get_data(3111) -> {2,{dog,8},[{locking,2}]};
get_data(3112) -> {2,{dog,9},[{coin,1800}]};
get_data(3113) -> {2,{dog,10},[{locking,2}]};
get_data(3114) -> {1,{monkey,4},[{coin,1580}]};
get_data(3115) -> {1,{monkey,5},[{locking,1}]};
get_data(3116) -> {1,{monkey,6},[{coin,2100}]};
get_data(3117) -> {1,{monkey,7},[{locking,1}]};
get_data(3118) -> {2,{monkey,8},[{coin,2500}]};
get_data(3119) -> {2,{monkey,9},[{trumpet,1}]};
get_data(3120) -> {2,{monkey,10},[{coin,3000}]};
get_data(3121) -> {1,{horse,4},[{ice,1}]};
get_data(3122) -> {1,{horse,5},[{coin,2800}]};
get_data(3123) -> {1,{horse,6},[{ice,1}]};
get_data(3124) -> {1,{horse,7},[{coin,3500}]};
get_data(3125) -> {2,{horse,8},[{auto,1}]};
get_data(3126) -> {2,{horse,9},[{trumpet,1}]};
get_data(3127) -> {2,{horse,10},[{rage,1}]};
get_data(3128) -> {1,{ox,3},[{gold,3}]};
get_data(3129) -> {2,{ox,4},[{gold,4}]};
get_data(3130) -> {2,{ox,5},[{gold,5}]};
get_data(3131) -> {2,{ox,6},[{gold,5}]};
get_data(3132) -> {2,{ox,7},[{gold,6}]};
get_data(3133) -> {3,{ox,8},[{gold,6}]};
get_data(3134) -> {3,{ox,9},[{gold,7}]};
get_data(3135) -> {3,{ox,10},[{gold,7}]};
get_data(3136) -> {1,{panda,3},[{gold,3}]};
get_data(3137) -> {2,{panda,4},[{gold,4}]};
get_data(3138) -> {2,{panda,5},[{gold,5}]};
get_data(3139) -> {3,{panda,6},[{gold,6}]};
get_data(3140) -> {3,{panda,7},[{gold,7}]};
get_data(3141) -> {4,{panda,8},[{gold,8}]};
get_data(3142) -> {4,{panda,9},[{gold,9}]};
get_data(3143) -> {4,{panda,10},[{horn,2}]};
get_data(3144) -> {4,{hippo,3},[{tel_fare,35}]};
get_data(3145) -> {5,{hippo,4},[{tel_fare,40}]};
get_data(3146) -> {5,{hippo,5},[{tel_fare,45}]};
get_data(3147) -> {6,{hippo,6},[{tel_fare,50}]};
get_data(3148) -> {6,{hippo,7},[{tel_fare,55}]};
get_data(3149) -> {7,{hippo,8},[{tel_fare,60}]};
get_data(3150) -> {5,{lion,3},[{gold,60}]};
get_data(3151) -> {6,{lion,4},[{gold,70}]};
get_data(3152) -> {6,{lion,5},[{gold,80}]};
get_data(3153) -> {6,{lion,6},[{gold,90}]};
get_data(3154) -> {7,{lion,7},[{gold,100}]};
get_data(3155) -> {7,{lion,8},[{gold,110}]};
get_data(3156) -> {6,{elephant,1},[{red_bag,100}]};
get_data(3157) -> {7,{elephant,2},[{red_bag,150}]};
get_data(3158) -> {7,{elephant,3},[{red_bag,200}]};
get_data(3159) -> {4,{pikachu,3},[{horn,1}]};
get_data(3160) -> {4,{pikachu,4},[{rage,1}]};
get_data(3161) -> {4,{pikachu,5},[{trumpet,2}]};
get_data(3162) -> {5,{pikachu,6},[{auto,2}]};
get_data(3163) -> {6,{pikachu,7},[{auto,2}]};
get_data(3164) -> {6,{pikachu,8},[{auto,2}]};
get_data(3165) -> {6,{bomber,1},[{red_bag,100}]};
get_data(3166) -> {7,{bomber,2},[{red_bag,150}]};
get_data(3167) -> {7,{bomber,3},[{red_bag,200}]};
get_data(3168) -> {3,{type_bomber,3},[{gold,4}]};
get_data(3169) -> {3,{type_bomber,4},[{gold,5}]};
get_data(3170) -> {3,{type_bomber,5},[{gold,6}]};
get_data(3171) -> {4,{type_bomber,6},[{gold,7}]};
get_data(3172) -> {4,{type_bomber,7},[{gold,8}]};
get_data(3173) -> {5,{type_bomber,8},[{gold,9}]};
get_data(3174) -> {3,{xsx,3},[{horn,1}]};
get_data(3175) -> {3,{xsx,4},[{rage,1}]};
get_data(3176) -> {4,{xsx,5},[{trumpet,2}]};
get_data(3177) -> {4,{xsx,6},[{auto,2}]};
get_data(3178) -> {4,{xsx,7},[{auto,2}]};
get_data(3179) -> {5,{xsx,8},[{auto,2}]};
get_data(3180) -> {3,{dsy,3},[{coin,8888}]};
get_data(3181) -> {3,{dsy,4},[{auto,1}]};
get_data(3182) -> {4,{dsy,5},[{coin,14800}]};
get_data(3183) -> {4,{dsy,6},[{auto,2}]};
get_data(3184) -> {4,{dsy,7},[{coin,24000}]};
get_data(3185) -> {5,{dsy,8},[{gold,30}]};
get_data(3186) -> {4,{area_bomber,2},[{coin,10000}]};
get_data(3187) -> {5,{area_bomber,3},[{coin,12000}]};
get_data(3188) -> {6,{area_bomber,4},[{coin,14000}]};
get_data(3189) -> {7,{area_bomber,5},[{coin,16000}]}.

