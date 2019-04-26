{application, main,
    [
        {description, "server"},
        {vsn, "0.1"},
        {modules, []},
        {registered, [main]},
        {applications, [kernel, stdlib]},
        {mod, {main, []}},
        {start_phases, []},
        {env, [
                {server_no, 1}
                ,{port, 8005}  %% 游戏端口
                ,{web_port, 10005} %%后台端口
                ,{game_ready, true} %%是否开放端口
                ,{gm, false} %%是否开放GM
                ,{ip, "127.0.0.1"}   %% 自己的ip
                ,{web_ip, [{192, 168, 0, 152}, {192, 168, 0, 123}, {120,76,28,170}, {134,175,80,79}, {127, 0, 0, 1}]}  %% 游戏后台ip

                %% 聚合信息
                ,{juhekey, ""} %% %%聚合礼品卡key
                ,{juheopenid, ""} %% %%聚合openid
                ,{juheacc, ""} %% 聚合账号
                ,{juhetpl, ""}  %% 聚合信息模板
                ,{juhesjkey, ""}  %% 聚合话费直冲key
                ,{juhemsgkey, ""} %% 聚合短信的key

                %% 登陆公众号
                ,{loginAppId, ""}                  
                ,{loginAppSecret, ""}
                %% 红包公众号，商户号，密匙
                ,{redAppId, ""}
                ,{redAppSecret, ""}
                ,{redMachId, ""}
                ,{redMachKey, ""}
                %% 官方支付公众号，商户号，密匙
                ,{payAppId, ""}
                ,{payAppSecret, ""}
                ,{payMachId, ""}
                ,{payMachKey, ""}
                %% 易宝
                ,{ybMachId, ""}
                ,{ybMachKey, ""}
                %% 摇钱树
                ,{yqsMachId, ""}
                ,{yqsMachKey, ""}
                %% 个人扫码
                ,{paysapi_uid, ""}
                ,{paysapi_token, ""}

                %% 机器人id列表
                ,{robot, []}

                ,{db_cfg,                          %% 数据库配置
                        {
                        "127.0.0.1"              %% ip
                        ,3306                        %% 端口
                        ,"root"                      %% 用户名
                        ,"123456"                    %% 密码
                        ,"magic_world"                      %% 表名
                        ,utf8mb4                     %% 数据格式
                        ,5                           %% 最小连接数
                        ,10                          %% 最大连接数
                      }
                }
            ]}
    ]
}.
