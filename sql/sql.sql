-- 注册用户
-- 人物数据
DROP TABLE IF EXISTS role;
CREATE TABLE `role` (
  `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
  `openid` varchar(50)  COMMENT '微信openid',
  `name` varchar(50)  COMMENT '名字',
  `icon` varchar(300)  COMMENT '头像',
  `phone` varchar(50)  COMMENT '电话号码',
  `phone_screat` varchar(50)  COMMENT '电话密码',
  `gold` int(11)  DEFAULT '0' COMMENT '钻石',
  `coin` int(11)  DEFAULT '0' COMMENT '金逗',
  `type` int(11)  DEFAULT '0' COMMENT '类型',
  `parent_id` int(11)  DEFAULT '0' COMMENT '上级id',
  `vip` int(11)  DEFAULT '0' COMMENT 'vip等级',
  `login_time` int(11)  DEFAULT '0' COMMENT '最后一次登陆的时间戳',
  `off_time` int(11)  DEFAULT '0' COMMENT '最后一次下线的时间戳',
  `charge` int(11)  DEFAULT '0' COMMENT '总充值',
  `regist_time` int(11)  DEFAULT '0' COMMENT '注册时间',
  `off` int(11)  DEFAULT '0' COMMENT '是否下线，0否，1是',
  `exchange` int(11)  DEFAULT '0' COMMENT '总兑换（分）',
  `lollipop` int(11)  DEFAULT '0' COMMENT '棒棒糖拥有个数',
  `red_bag` int(11)  DEFAULT '0' COMMENT '红包值（分）',
  `tel_fare` int(11)  DEFAULT '0' COMMENT '话费值（毛）',
  `channel` int(11)  DEFAULT '0' COMMENT '合作商id',
  `pay_openid` varchar(50)  COMMENT '支付openid',
  `red_openid` varchar(50)  COMMENT '红包openid',
  `max_coin` int(11)  DEFAULT '1' COMMENT '最大挡位',
  `candy` int(11)  DEFAULT '0' COMMENT '糖果',
  `lolly` int(11)  DEFAULT '0' COMMENT '中棒棒糖',
  `info` text  COMMENT '玩家所有信息',
  PRIMARY KEY (`role_id`),
  KEY `openid` (`openid`),
  KEY `channel` (`channel`),
  KEY `parent_id` (`parent_id`),
  KEY `phone` (`phone`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 邮件日志
DROP TABLE IF EXISTS mail_log;
CREATE TABLE `mail_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id, 邮件唯一id',
    `from_id` int(11) DEFAULT '0'  COMMENT '发件人人物id',
    `to_id` int(11) DEFAULT '0'  COMMENT '收件人物id',
    `items` varchar(200)  COMMENT '物品列表',
    `time` int(11)  DEFAULT '0' COMMENT '发送时间',
    `reward_time` int(11)  DEFAULT '0' COMMENT '领取时间',
    `status` int(11)  DEFAULT '0' COMMENT '状态， 0未领取，1已经领取, 2已经删除',
    `delete_time` int(11)  DEFAULT '0' COMMENT '删除时间',
    PRIMARY KEY (`id`),
    KEY `from_id` (`from_id`),
    KEY `to_id` (`to_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 京东卡兑换日志
DROP TABLE IF EXISTS jd_card;
CREATE TABLE `jd_card` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '消费类型(1，金币，2钻石，3，棒棒糖, 4话费券)',
    `value` int(11) DEFAULT '0'  COMMENT '消费值',
    `cami` varchar(50)  COMMENT '京东卡密',
    `price` int(11) DEFAULT '0'  COMMENT '价值(元)',
    `time` int(11) DEFAULT '0'  COMMENT '兑换时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 话费兑换日志
DROP TABLE IF EXISTS phone_card;
CREATE TABLE `phone_card` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '消费类型(1，金币，2钻石，3，棒棒糖， 4话费券)',
    `value` int(11) DEFAULT '0'  COMMENT '消费值',
    `cami` varchar(50)  COMMENT '电话卡卡密',
    `phone` varchar(50)  COMMENT '电话号码',
    `price` int(11) DEFAULT '0'  COMMENT '价值(元)',
    `time` int(11) DEFAULT '0'  COMMENT '兑换时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 充值日志
DROP TABLE IF EXISTS charge_log;
CREATE TABLE `charge_log` (
    `id` varchar(50) NOT NULL  COMMENT '订单id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `charge_rmb` int(11) DEFAULT '0'  COMMENT '充值金额（分）',
    `charge_type` int(11) DEFAULT '0'  COMMENT '充值支付类型，0微信官方，1威富通，2摇钱树支付宝',
    `type` int(11) DEFAULT '0'  COMMENT '充值类型(1,金币，2钻石, 3,1元礼包，4，贵族礼包)',
    `value` int(11) DEFAULT '0'  COMMENT '充值获得的值',
    `send` int(11) DEFAULT '0'  COMMENT '充值赠送的值',
    `time` int(11) DEFAULT '0'  COMMENT '充值时间',
    `status` int(11) DEFAULT '0'  COMMENT '订单状态0,未完成 1，成功',
    `call_time` int(11) DEFAULT '0'  COMMENT '充值回调时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `type` (`type`),
    KEY `multi`(`time`,`role_id`, `status`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 金币获得和消费日志
DROP TABLE IF EXISTS coin_cost_log;
CREATE TABLE `coin_cost_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `add_value` int(11) DEFAULT '0'  COMMENT '操作获得的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role` (`role_id`, `time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 钻石获得和消费日志
DROP TABLE IF EXISTS gold_cost_log;
CREATE TABLE `gold_cost_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `add_value` int(11) DEFAULT '0'  COMMENT '操作获得的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 红包兑换记录
DROP TABLE IF EXISTS red_packet_log;
CREATE TABLE `red_packet_log` (
  `order_id` varchar(50) NOT NULL COMMENT '订单id',
  `role_id` int(11) NOT NULL COMMENT '人物id',
  `cny` int(11) NOT NULL COMMENT '兑换的钱（分）',
  `status` int(11) DEFAULT '0' COMMENT '兑换的钱（分）',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`order_id`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 每日任务
DROP TABLE IF EXISTS daily_task;
CREATE TABLE `daily_task` (
  `id` int(11) NOT NULL COMMENT '只有一个id',
  `list` text NOT NULL COMMENT '任务列表',
  `time` int(11) NOT NULL COMMENT '任务时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;



-- 合作商关系表
DROP TABLE IF EXISTS channel;
CREATE TABLE `channel` (
  `id` int(11) NOT NULL COMMENT '合作商id',
  `role_id` int(11) NOT NULL COMMENT '玩家id',
  `bill_total` int(10) unsigned DEFAULT '0' COMMENT '合作商账单利润金额的汇总',
  `bill_nums` int(11) DEFAULT '0' COMMENT '账单的记录总数',
  `charge_in_month` int(11) DEFAULT '0' COMMENT '首月充值金额，单位分',
  `unfinished_charge` int(11) DEFAULT '0' COMMENT '未完成任务账单的累计充值金额，单位分',
  `task_times` tinyint(4) DEFAULT '0' COMMENT '没有完成推广任务的次数统计',
  `settlement` int(11) DEFAULT '0' COMMENT '合作商已结算金额，单位：分',
  `username` varchar(30) DEFAULT '' COMMENT '合作商账号',
  `nick` varchar(30) DEFAULT '' COMMENT '合作商昵称',
  `created_at` int(11) DEFAULT '0' COMMENT '创建时间',
  `settle_at` int(11) DEFAULT '0' COMMENT '最后一期的结算时间',
  `qrcode` varchar(200) DEFAULT '' COMMENT '推广二维码',
  `charge_val` int(11) NOT NULL COMMENT '推广任务的充值金额，单位分',
  `profit_val` int(11) NOT NULL COMMENT '推广任务的利润金额，单位分',
  `profit_ratio` tinyint(4) DEFAULT '0' COMMENT '合作商的利润比例',
  PRIMARY KEY (`id`),
  UNIQUE KEY `role_id` (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


-- 红包获取日志
DROP TABLE IF EXISTS red_bag_log;
CREATE TABLE `red_bag_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `time_roleid` (`time`, `role_Id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 棒棒糖获取日志
DROP TABLE IF EXISTS lollipop_log;
CREATE TABLE `lollipop_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 话费获取日志
DROP TABLE IF EXISTS tel_fare_log;
CREATE TABLE `tel_fare_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 合作商充值记录
DROP TABLE IF EXISTS channel_charge_log;
CREATE TABLE `channel_charge_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `channel_id` int(11) DEFAULT '0'  COMMENT '合作商id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `charge_rmb` int(11) DEFAULT '0'  COMMENT '充值金额（分）',
    `type` int(11) DEFAULT '0'  COMMENT '充值类型(1,金币，2钻石, 3,1元礼包，4，贵族礼包)',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `channel_id` (`channel_id`, `time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 合作商兑换记录
DROP TABLE IF EXISTS channel_exchange_log;
CREATE TABLE `channel_exchange_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `channel_id` int(11) DEFAULT '0'  COMMENT '合作商id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `exchange` int(11) DEFAULT '0'  COMMENT '兑换价值（分）',
    `type` int(11) DEFAULT '0'  COMMENT '兑换类型(1,话费，2京东卡，3红包)',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `channel_id` (`channel_id`, `time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 糖果获取日志
DROP TABLE IF EXISTS candy_log;
CREATE TABLE `candy_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 中棒棒糖获取日志
DROP TABLE IF EXISTS lolly_log;
CREATE TABLE `lolly_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 竞技场日志
DROP TABLE IF EXISTS area_log;
CREATE TABLE `area_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `all_num` int(11) DEFAULT '0'  COMMENT '总次数',
    `free_num` int(11) DEFAULT '0'  COMMENT '免费次数',
    `pay_num` int(11) DEFAULT '0'  COMMENT '付费次数',
    `pay_coin` int(11) DEFAULT '0'  COMMENT '付费金币',
    `reward` int(11) DEFAULT '0'  COMMENT '总奖励金币',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 玩家注册留存日志
DROP TABLE IF EXISTS role_account_log;
CREATE TABLE `role_account_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `channel` int(11) DEFAULT '0' COMMENT '合作商id' ,
    `registe` int(11) DEFAULT '0'  COMMENT '注册人数',
    `login` int(11) DEFAULT '0'  COMMENT '登陆人数',
    `next_day` int(11) DEFAULT '0'  COMMENT '次日留存',
    `seven_day` int(11) DEFAULT '0'  COMMENT '7日留存',
    `fifteen_day` int(11) DEFAULT '0'  COMMENT '15日留存',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`),
    KEY `channel`(`channel`, `time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 动物园产出日志
DROP TABLE IF EXISTS animal_account_log;
CREATE TABLE `animal_account_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `p` bigint(11) DEFAULT '0'  COMMENT '总投入金币',
    `w` bigint(11) DEFAULT '0'  COMMENT '总产出金币',
    `lollipop` int(11) DEFAULT '0'  COMMENT '总产出大棒棒糖数量',
    `lolly` int(11) DEFAULT '0'  COMMENT '总产出中棒棒糖数量',
    `candy` int(11) DEFAULT '0'  COMMENT '总产出小棒棒糖数量',
    `red_bag` int(11) DEFAULT '0'  COMMENT '红包总产生(毛)',
    `task_coin` int(11) DEFAULT '0'  COMMENT '每日任务产出金币',
    `task_tel` int(11) DEFAULT '0'  COMMENT '每日任务产出话费(毛)',
    `bonus_coin` int(11) DEFAULT '0'  COMMENT '彩金金币',
    `bonus_lollipop` int(11) DEFAULT '0'  COMMENT '彩金大棒棒糖',
    `bonus_tel` int(11) DEFAULT '0'  COMMENT '彩金话费(毛)',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 充值等账目日志
DROP TABLE IF EXISTS account_log;
CREATE TABLE `account_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `coin_charge` bigint(11) DEFAULT '0'  COMMENT '金币充值（分）',
    `first_charge` bigint(11) DEFAULT '0'  COMMENT '1元礼包充值（分）',
    `gift_charge` int(11) DEFAULT '0'  COMMENT '贵族礼包充值（分）',
    `gold_charge` int(11) DEFAULT '0'  COMMENT '钻石充值（分）',
    `charge_coin` bigint(11) DEFAULT '0'  COMMENT '金币充值产出金币',
    `first_coin` int(11) DEFAULT '0'  COMMENT '1元礼包充值产出金币',
    `share_coin` int(11) DEFAULT '0'  COMMENT '分享产出金币',
    `invite_coin` int(11) DEFAULT '0'  COMMENT '邀请好友产出金币',
    `alms_coin` int(11) DEFAULT '0'  COMMENT '每日救济金金币',
    `vip_alms_coin` int(11) DEFAULT '0'  COMMENT 'vip救济金金币',
    `open_fire` int(11) DEFAULT '0'  COMMENT '解锁挡位产出金币',
    `gift_coin` int(11) DEFAULT '0'  COMMENT '贵族礼包产出金币',
    `area_coin` int(11) DEFAULT '0'  COMMENT '竞技场产出金币',
    `id_card_coin` int(11) DEFAULT '0'  COMMENT '身份证亚洲产出金币',
    `guide_task` int(11) DEFAULT '0'  COMMENT '新手任务产出金币',
    `guide_login_coin` int(11) DEFAULT '0'  COMMENT '新手礼包产出金币',
    `guide_login_tel` int(11) DEFAULT '0'  COMMENT '新手礼包产出话费券（毛）',
    `login_coin` int(11) DEFAULT '0'  COMMENT '每日登陆转盘产出金币',
    `login_tel` int(11) DEFAULT '0'  COMMENT '每日登陆转盘产出话费券（毛）',
    `vip_login_coin` int(11) DEFAULT '0'  COMMENT 'vip登陆转盘产出金币',
    `vip_login_tel` int(11) DEFAULT '0'  COMMENT 'vip登陆转盘产出话费券（毛）',
    `gift_code_coin` int(11) DEFAULT '0'  COMMENT '礼品码产出金币',
    `gift_code_redbag` int(11) DEFAULT '0'  COMMENT '礼品码产出红包(毛)',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 前端错误日志
DROP TABLE IF EXISTS client_error_log;
CREATE TABLE `client_error_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` int(11) NOT NULL COMMENT '人物id',
  `name` varchar(50)  COMMENT '名字',
  `msg` text COMMENT '错误日志',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 在线人数统计
DROP TABLE IF EXISTS role_online_log;
CREATE TABLE `role_online_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `num` int(11) NOT NULL COMMENT '在线人数',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 摇钱树活动产出
DROP TABLE IF EXISTS coin_tree_log;
CREATE TABLE `coin_tree_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` int(11) NOT NULL COMMENT '玩家id',
  `cost_coin` int(11) NOT NULL COMMENT '投入金币',
  `add_coin` int(11) NOT NULL COMMENT '产生金币',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 送分减分日志
DROP TABLE IF EXISTS send_coin_log;
CREATE TABLE `send_coin_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` int(11) NOT NULL COMMENT '玩家id',
  `type` int(11) NOT NULL COMMENT '类型，1 金币， 2钻石',
  `num` int(11) NOT NULL COMMENT '改变数量',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 大奖赛统计
DROP TABLE IF EXISTS great_account_log;
CREATE TABLE `great_account_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `p` bigint(11) DEFAULT '0'  COMMENT '总投入金币',
    `w` bigint(11) DEFAULT '0'  COMMENT '总产出金币',
    `all_num` int(11) DEFAULT '0'  COMMENT '总报名次数',
    `free_num` int(11) DEFAULT '0'  COMMENT '免费报名次数',
    `cost_num` int(11) DEFAULT '0'  COMMENT '消费报名次数',
    `cost_value` int(11) DEFAULT '0'  COMMENT '消费钻石数量',
    `cost_role_num` int(11) DEFAULT '0'  COMMENT '消费报名人数',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 兑换流量 
DROP TABLE IF EXISTS exchange_flow;
CREATE TABLE `exchange_flow` (
    `id` int(11) NOT NULL COMMENT 'id',
    `flow` int(11) DEFAULT '0'  COMMENT '兑换剩余流量',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 红包抵扣充值
DROP TABLE IF EXISTS redbag_to_charge;
CREATE TABLE `redbag_to_charge` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `redbag` int(11) DEFAULT '0'  COMMENT '红包抵扣的金额（分）',
    `type` int(11) DEFAULT '0'  COMMENT '兑换类型',
    `value` int(11) DEFAULT '0'  COMMENT '获得值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 活动掉落
DROP TABLE IF EXISTS active_drop_log;
CREATE TABLE `active_drop_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `coin` int(11) DEFAULT '0'  COMMENT '打动物的金币',
    `rate` int(11) DEFAULT '0'  COMMENT '动物的倍数 * 10',
    `item` varchar(50)  COMMENT '掉落物品名字',
    `value` int(11) DEFAULT '0'  COMMENT '价值（元）',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 商城兑换
DROP TABLE IF EXISTS shop_exchange_log;
CREATE TABLE `shop_exchange_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `cost_type` int(11) DEFAULT '0'  COMMENT '兑换消耗的类型，1波板糖',
    `cost_num` int(11) DEFAULT '0'  COMMENT '兑换消耗的数量',
    `exchange_type` int(11) DEFAULT '0'  COMMENT '兑换的类型，1：京东卡，2：话费，3：钻石',
    `exchange_price` int(11) DEFAULT '0'  COMMENT '兑换的总价值（元）',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 每日人物充值兑换统计
DROP TABLE IF EXISTS role_charge_exchange_log;
CREATE TABLE `role_charge_exchange_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `channel` int(11) DEFAULT '0'  COMMENT '渠道id',
    `charge` int(11) DEFAULT '0'  COMMENT '充值（分）',
    `exchange` int(11) DEFAULT '0'  COMMENT '兑换（分）',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;
