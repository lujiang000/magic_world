-ifndef(M_1001_TOS_PB_H).
-define(M_1001_TOS_PB_H, true).
-record(m_1001_tos, {
    code = erlang:error({required, code}),
    parent_id = erlang:error({required, parent_id}),
    red_code = erlang:error({required, red_code}),
    pay_code = erlang:error({required, pay_code}),
    channel_id = erlang:error({required, channel_id}),
    role_id = erlang:error({required, role_id}),
    flag = erlang:error({required, flag})
}).
-endif.

-ifndef(M_1001_TOC_PB_H).
-define(M_1001_TOC_PB_H, true).
-record(m_1001_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1002_TOS_PB_H).
-define(M_1002_TOS_PB_H, true).
-record(m_1002_tos, {
    account = erlang:error({required, account})
}).
-endif.

-ifndef(M_1002_TOC_PB_H).
-define(M_1002_TOC_PB_H, true).
-record(m_1002_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1003_TOS_PB_H).
-define(M_1003_TOS_PB_H, true).
-record(m_1003_tos, {
    role_id = erlang:error({required, role_id}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1003_TOC_PB_H).
-define(M_1003_TOC_PB_H, true).
-record(m_1003_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1004_TOS_PB_H).
-define(M_1004_TOS_PB_H, true).
-record(m_1004_tos, {
    phone = erlang:error({required, phone}),
    screat = erlang:error({required, screat})
}).
-endif.

-ifndef(M_1004_TOC_PB_H).
-define(M_1004_TOC_PB_H, true).
-record(m_1004_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1005_TOS_PB_H).
-define(M_1005_TOS_PB_H, true).
-record(m_1005_tos, {
    phone = erlang:error({required, phone})
}).
-endif.

-ifndef(M_1005_TOC_PB_H).
-define(M_1005_TOC_PB_H, true).
-record(m_1005_toc, {
    
}).
-endif.

-ifndef(M_1006_TOS_PB_H).
-define(M_1006_TOS_PB_H, true).
-record(m_1006_tos, {
    phone = erlang:error({required, phone}),
    screat = erlang:error({required, screat}),
    id = erlang:error({required, id}),
    parent_id = erlang:error({required, parent_id}),
    channel_id = erlang:error({required, channel_id})
}).
-endif.

-ifndef(M_1006_TOC_PB_H).
-define(M_1006_TOC_PB_H, true).
-record(m_1006_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(P_ROLE_INFO_PB_H).
-define(P_ROLE_INFO_PB_H, true).
-record(p_role_info, {
    role_id = erlang:error({required, role_id}),
    nick_name = erlang:error({required, nick_name}),
    icon = erlang:error({required, icon}),
    vip = erlang:error({required, vip}),
    gold = erlang:error({required, gold}),
    coin = erlang:error({required, coin}),
    status = erlang:error({required, status}),
    use_coin = erlang:error({required, use_coin}),
    first_gift = erlang:error({required, first_gift}),
    id_card = erlang:error({required, id_card}),
    sign = erlang:error({required, sign}),
    phone = erlang:error({required, phone}),
    channel = erlang:error({required, channel}),
    openid = erlang:error({required, openid}),
    regist,
    vip_effect = erlang:error({required, vip_effect}),
    subscribe = erlang:error({required, subscribe})
}).
-endif.

-ifndef(M_1098_TOS_PB_H).
-define(M_1098_TOS_PB_H, true).
-record(m_1098_tos, {
    
}).
-endif.

-ifndef(M_1098_TOC_PB_H).
-define(M_1098_TOC_PB_H, true).
-record(m_1098_toc, {
    
}).
-endif.

-ifndef(M_1099_TOC_PB_H).
-define(M_1099_TOC_PB_H, true).
-record(m_1099_toc, {
    error_code = erlang:error({required, error_code})
}).
-endif.

-ifndef(M_1101_TOC_PB_H).
-define(M_1101_TOC_PB_H, true).
-record(m_1101_toc, {
    list = []
}).
-endif.

-ifndef(P_ASSETS_PB_H).
-define(P_ASSETS_PB_H, true).
-record(p_assets, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1102_TOS_PB_H).
-define(M_1102_TOS_PB_H, true).
-record(m_1102_tos, {
    
}).
-endif.

-ifndef(M_1102_TOC_PB_H).
-define(M_1102_TOC_PB_H, true).
-record(m_1102_toc, {
    list = []
}).
-endif.

-ifndef(M_1103_TOS_PB_H).
-define(M_1103_TOS_PB_H, true).
-record(m_1103_tos, {
    id = erlang:error({required, id}),
    name = erlang:error({required, name})
}).
-endif.

-ifndef(M_1103_TOC_PB_H).
-define(M_1103_TOC_PB_H, true).
-record(m_1103_toc, {
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(M_1104_TOS_PB_H).
-define(M_1104_TOS_PB_H, true).
-record(m_1104_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1104_TOC_PB_H).
-define(M_1104_TOC_PB_H, true).
-record(m_1104_toc, {
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(M_1105_TOS_PB_H).
-define(M_1105_TOS_PB_H, true).
-record(m_1105_tos, {
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1105_TOC_PB_H).
-define(M_1105_TOC_PB_H, true).
-record(m_1105_toc, {
    
}).
-endif.

-ifndef(M_1106_TOC_PB_H).
-define(M_1106_TOC_PB_H, true).
-record(m_1106_toc, {
    type = erlang:error({required, type}),
    name = erlang:error({required, name}),
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1107_TOS_PB_H).
-define(M_1107_TOS_PB_H, true).
-record(m_1107_tos, {
    type = erlang:error({required, type}),
    phone
}).
-endif.

-ifndef(M_1107_TOC_PB_H).
-define(M_1107_TOC_PB_H, true).
-record(m_1107_toc, {
    
}).
-endif.

-ifndef(M_1108_TOS_PB_H).
-define(M_1108_TOS_PB_H, true).
-record(m_1108_tos, {
    num = erlang:error({required, num}),
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1108_TOC_PB_H).
-define(M_1108_TOC_PB_H, true).
-record(m_1108_toc, {
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(M_1109_TOS_PB_H).
-define(M_1109_TOS_PB_H, true).
-record(m_1109_tos, {
    role_id = erlang:error({required, role_id}),
    num = erlang:error({required, num}),
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1109_TOC_PB_H).
-define(M_1109_TOC_PB_H, true).
-record(m_1109_toc, {
    
}).
-endif.

-ifndef(M_1110_TOS_PB_H).
-define(M_1110_TOS_PB_H, true).
-record(m_1110_tos, {
    num = erlang:error({required, num}),
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1110_TOC_PB_H).
-define(M_1110_TOC_PB_H, true).
-record(m_1110_toc, {
    
}).
-endif.

-ifndef(M_1111_TOS_PB_H).
-define(M_1111_TOS_PB_H, true).
-record(m_1111_tos, {
    
}).
-endif.

-ifndef(M_1111_TOC_PB_H).
-define(M_1111_TOC_PB_H, true).
-record(m_1111_toc, {
    list = []
}).
-endif.

-ifndef(M_1112_TOS_PB_H).
-define(M_1112_TOS_PB_H, true).
-record(m_1112_tos, {
    
}).
-endif.

-ifndef(M_1112_TOC_PB_H).
-define(M_1112_TOC_PB_H, true).
-record(m_1112_toc, {
    buy_time = erlang:error({required, buy_time}),
    end_time = erlang:error({required, end_time}),
    reward = erlang:error({required, reward})
}).
-endif.

-ifndef(M_1113_TOS_PB_H).
-define(M_1113_TOS_PB_H, true).
-record(m_1113_tos, {
    
}).
-endif.

-ifndef(M_1113_TOC_PB_H).
-define(M_1113_TOC_PB_H, true).
-record(m_1113_toc, {
    list = []
}).
-endif.

-ifndef(P_SEND_LOG_PB_H).
-define(P_SEND_LOG_PB_H, true).
-record(p_send_log, {
    role_id = erlang:error({required, role_id}),
    type = erlang:error({required, type}),
    num = erlang:error({required, num}),
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1114_TOS_PB_H).
-define(M_1114_TOS_PB_H, true).
-record(m_1114_tos, {
    
}).
-endif.

-ifndef(M_1114_TOC_PB_H).
-define(M_1114_TOC_PB_H, true).
-record(m_1114_toc, {
    list = []
}).
-endif.

-ifndef(P_USE_LOG_PB_H).
-define(P_USE_LOG_PB_H, true).
-record(p_use_log, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num}),
    type1 = erlang:error({required, type1}),
    num1 = erlang:error({required, num1}),
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1115_TOS_PB_H).
-define(M_1115_TOS_PB_H, true).
-record(m_1115_tos, {
    
}).
-endif.

-ifndef(M_1115_TOC_PB_H).
-define(M_1115_TOC_PB_H, true).
-record(m_1115_toc, {
    list = []
}).
-endif.

-ifndef(M_1116_TOS_PB_H).
-define(M_1116_TOS_PB_H, true).
-record(m_1116_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1116_TOC_PB_H).
-define(M_1116_TOC_PB_H, true).
-record(m_1116_toc, {
    name = erlang:error({required, name}),
    sign = erlang:error({required, sign})
}).
-endif.

-ifndef(M_1117_TOS_PB_H).
-define(M_1117_TOS_PB_H, true).
-record(m_1117_tos, {
    
}).
-endif.

-ifndef(M_1117_TOC_PB_H).
-define(M_1117_TOC_PB_H, true).
-record(m_1117_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1118_TOS_PB_H).
-define(M_1118_TOS_PB_H, true).
-record(m_1118_tos, {
    
}).
-endif.

-ifndef(M_1118_TOC_PB_H).
-define(M_1118_TOC_PB_H, true).
-record(m_1118_toc, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1119_TOS_PB_H).
-define(M_1119_TOS_PB_H, true).
-record(m_1119_tos, {
    
}).
-endif.

-ifndef(M_1119_TOC_PB_H).
-define(M_1119_TOC_PB_H, true).
-record(m_1119_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1120_TOS_PB_H).
-define(M_1120_TOS_PB_H, true).
-record(m_1120_tos, {
    
}).
-endif.

-ifndef(M_1120_TOC_PB_H).
-define(M_1120_TOC_PB_H, true).
-record(m_1120_toc, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1121_TOS_PB_H).
-define(M_1121_TOS_PB_H, true).
-record(m_1121_tos, {
    
}).
-endif.

-ifndef(M_1121_TOC_PB_H).
-define(M_1121_TOC_PB_H, true).
-record(m_1121_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1122_TOS_PB_H).
-define(M_1122_TOS_PB_H, true).
-record(m_1122_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num}),
    charge_type = erlang:error({required, charge_type}),
    return_url
}).
-endif.

-ifndef(M_1122_TOC_PB_H).
-define(M_1122_TOC_PB_H, true).
-record(m_1122_toc, {
    info = erlang:error({required, info}),
    charge_type = erlang:error({required, charge_type})
}).
-endif.

-ifndef(M_1123_TOC_PB_H).
-define(M_1123_TOC_PB_H, true).
-record(m_1123_toc, {
    type = erlang:error({required, type}),
    list = []
}).
-endif.

-ifndef(M_1124_TOC_PB_H).
-define(M_1124_TOC_PB_H, true).
-record(m_1124_toc, {
    vip = erlang:error({required, vip})
}).
-endif.

-ifndef(M_1125_TOS_PB_H).
-define(M_1125_TOS_PB_H, true).
-record(m_1125_tos, {
    
}).
-endif.

-ifndef(M_1125_TOC_PB_H).
-define(M_1125_TOC_PB_H, true).
-record(m_1125_toc, {
    vip = erlang:error({required, vip}),
    charge = erlang:error({required, charge})
}).
-endif.

-ifndef(M_1126_TOS_PB_H).
-define(M_1126_TOS_PB_H, true).
-record(m_1126_tos, {
    
}).
-endif.

-ifndef(M_1126_TOC_PB_H).
-define(M_1126_TOC_PB_H, true).
-record(m_1126_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1127_TOS_PB_H).
-define(M_1127_TOS_PB_H, true).
-record(m_1127_tos, {
    
}).
-endif.

-ifndef(M_1127_TOC_PB_H).
-define(M_1127_TOC_PB_H, true).
-record(m_1127_toc, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1128_TOS_PB_H).
-define(M_1128_TOS_PB_H, true).
-record(m_1128_tos, {
    type = erlang:error({required, type}),
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1128_TOC_PB_H).
-define(M_1128_TOC_PB_H, true).
-record(m_1128_toc, {
    
}).
-endif.

-ifndef(M_1130_TOS_PB_H).
-define(M_1130_TOS_PB_H, true).
-record(m_1130_tos, {
    phone = erlang:error({required, phone})
}).
-endif.

-ifndef(M_1130_TOC_PB_H).
-define(M_1130_TOC_PB_H, true).
-record(m_1130_toc, {
    
}).
-endif.

-ifndef(M_1131_TOS_PB_H).
-define(M_1131_TOS_PB_H, true).
-record(m_1131_tos, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(M_1131_TOC_PB_H).
-define(M_1131_TOC_PB_H, true).
-record(m_1131_toc, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num}),
    phone = erlang:error({required, phone})
}).
-endif.

-ifndef(M_1132_TOS_PB_H).
-define(M_1132_TOS_PB_H, true).
-record(m_1132_tos, {
    
}).
-endif.

-ifndef(M_1132_TOC_PB_H).
-define(M_1132_TOC_PB_H, true).
-record(m_1132_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1133_TOS_PB_H).
-define(M_1133_TOS_PB_H, true).
-record(m_1133_tos, {
    
}).
-endif.

-ifndef(M_1133_TOC_PB_H).
-define(M_1133_TOC_PB_H, true).
-record(m_1133_toc, {
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(M_1134_TOS_PB_H).
-define(M_1134_TOS_PB_H, true).
-record(m_1134_tos, {
    
}).
-endif.

-ifndef(M_1134_TOC_PB_H).
-define(M_1134_TOC_PB_H, true).
-record(m_1134_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1135_TOS_PB_H).
-define(M_1135_TOS_PB_H, true).
-record(m_1135_tos, {
    
}).
-endif.

-ifndef(M_1135_TOC_PB_H).
-define(M_1135_TOC_PB_H, true).
-record(m_1135_toc, {
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(M_1136_TOS_PB_H).
-define(M_1136_TOS_PB_H, true).
-record(m_1136_tos, {
    url = erlang:error({required, url})
}).
-endif.

-ifndef(M_1136_TOC_PB_H).
-define(M_1136_TOC_PB_H, true).
-record(m_1136_toc, {
    sign = erlang:error({required, sign}),
    timestamp = erlang:error({required, timestamp}),
    noncestr = erlang:error({required, noncestr})
}).
-endif.

-ifndef(M_1137_TOS_PB_H).
-define(M_1137_TOS_PB_H, true).
-record(m_1137_tos, {
    
}).
-endif.

-ifndef(M_1137_TOC_PB_H).
-define(M_1137_TOC_PB_H, true).
-record(m_1137_toc, {
    
}).
-endif.

-ifndef(M_1138_TOS_PB_H).
-define(M_1138_TOS_PB_H, true).
-record(m_1138_tos, {
    
}).
-endif.

-ifndef(M_1138_TOC_PB_H).
-define(M_1138_TOC_PB_H, true).
-record(m_1138_toc, {
    num = erlang:error({required, num}),
    min = erlang:error({required, min})
}).
-endif.

-ifndef(M_1139_TOS_PB_H).
-define(M_1139_TOS_PB_H, true).
-record(m_1139_tos, {
    
}).
-endif.

-ifndef(M_1139_TOC_PB_H).
-define(M_1139_TOC_PB_H, true).
-record(m_1139_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1140_TOS_PB_H).
-define(M_1140_TOS_PB_H, true).
-record(m_1140_tos, {
    
}).
-endif.

-ifndef(M_1140_TOC_PB_H).
-define(M_1140_TOC_PB_H, true).
-record(m_1140_toc, {
    
}).
-endif.

-ifndef(M_1141_TOS_PB_H).
-define(M_1141_TOS_PB_H, true).
-record(m_1141_tos, {
    page = erlang:error({required, page}),
    num = erlang:error({required, num}),
    page_num = erlang:error({required, page_num})
}).
-endif.

-ifndef(M_1141_TOC_PB_H).
-define(M_1141_TOC_PB_H, true).
-record(m_1141_toc, {
    coin = erlang:error({required, coin}),
    num = erlang:error({required, num}),
    allpage = erlang:error({required, allpage}),
    list = [],
    setting = []
}).
-endif.

-ifndef(P_FRIEND_PB_H).
-define(P_FRIEND_PB_H, true).
-record(p_friend, {
    role_id = erlang:error({required, role_id}),
    icon = erlang:error({required, icon}),
    name = erlang:error({required, name})
}).
-endif.

-ifndef(M_1142_TOS_PB_H).
-define(M_1142_TOS_PB_H, true).
-record(m_1142_tos, {
    
}).
-endif.

-ifndef(M_1142_TOC_PB_H).
-define(M_1142_TOC_PB_H, true).
-record(m_1142_toc, {
    today = erlang:error({required, today}),
    tomorrow = erlang:error({required, tomorrow})
}).
-endif.

-ifndef(M_1143_TOS_PB_H).
-define(M_1143_TOS_PB_H, true).
-record(m_1143_tos, {
    
}).
-endif.

-ifndef(M_1143_TOC_PB_H).
-define(M_1143_TOC_PB_H, true).
-record(m_1143_toc, {
    
}).
-endif.

-ifndef(M_1144_TOS_PB_H).
-define(M_1144_TOS_PB_H, true).
-record(m_1144_tos, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(M_1144_TOC_PB_H).
-define(M_1144_TOC_PB_H, true).
-record(m_1144_toc, {
    list = []
}).
-endif.

-ifndef(M_1145_TOS_PB_H).
-define(M_1145_TOS_PB_H, true).
-record(m_1145_tos, {
    
}).
-endif.

-ifndef(M_1145_TOC_PB_H).
-define(M_1145_TOC_PB_H, true).
-record(m_1145_toc, {
    list = []
}).
-endif.

-ifndef(P_TALK_ROLE_PB_H).
-define(P_TALK_ROLE_PB_H, true).
-record(p_talk_role, {
    role_id = erlang:error({required, role_id}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon}),
    vip = erlang:error({required, vip}),
    status = erlang:error({required, status})
}).
-endif.

-ifndef(M_1146_TOS_PB_H).
-define(M_1146_TOS_PB_H, true).
-record(m_1146_tos, {
    message = erlang:error({required, message}),
    role_id = erlang:error({required, role_id})
}).
-endif.

-ifndef(M_1146_TOC_PB_H).
-define(M_1146_TOC_PB_H, true).
-record(m_1146_toc, {
    
}).
-endif.

-ifndef(M_1147_TOC_PB_H).
-define(M_1147_TOC_PB_H, true).
-record(m_1147_toc, {
    role_id = erlang:error({required, role_id}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon}),
    vip = erlang:error({required, vip}),
    message = erlang:error({required, message})
}).
-endif.

-ifndef(M_1148_TOC_PB_H).
-define(M_1148_TOC_PB_H, true).
-record(m_1148_toc, {
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1149_TOC_PB_H).
-define(M_1149_TOC_PB_H, true).
-record(m_1149_toc, {
    start_time = erlang:error({required, start_time}),
    end_time = erlang:error({required, end_time})
}).
-endif.

-ifndef(M_1150_TOS_PB_H).
-define(M_1150_TOS_PB_H, true).
-record(m_1150_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1150_TOC_PB_H).
-define(M_1150_TOC_PB_H, true).
-record(m_1150_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1151_TOS_PB_H).
-define(M_1151_TOS_PB_H, true).
-record(m_1151_tos, {
    num = erlang:error({required, num}),
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1151_TOC_PB_H).
-define(M_1151_TOC_PB_H, true).
-record(m_1151_toc, {
    
}).
-endif.

-ifndef(M_1152_TOS_PB_H).
-define(M_1152_TOS_PB_H, true).
-record(m_1152_tos, {
    vip = erlang:error({required, vip})
}).
-endif.

-ifndef(M_1152_TOC_PB_H).
-define(M_1152_TOC_PB_H, true).
-record(m_1152_toc, {
    
}).
-endif.

-ifndef(M_1153_TOC_PB_H).
-define(M_1153_TOC_PB_H, true).
-record(m_1153_toc, {
    num = erlang:error({required, num}),
    role_id = erlang:error({required, role_id}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon})
}).
-endif.

-ifndef(M_1154_TOS_PB_H).
-define(M_1154_TOS_PB_H, true).
-record(m_1154_tos, {
    
}).
-endif.

-ifndef(M_1154_TOC_PB_H).
-define(M_1154_TOC_PB_H, true).
-record(m_1154_toc, {
    list = []
}).
-endif.

-ifndef(P_ACHIEVEMENT_PB_H).
-define(P_ACHIEVEMENT_PB_H, true).
-record(p_achievement, {
    type = erlang:error({required, type}),
    id = erlang:error({required, id}),
    reward_list = [],
    value = erlang:error({required, value})
}).
-endif.

-ifndef(M_1155_TOS_PB_H).
-define(M_1155_TOS_PB_H, true).
-record(m_1155_tos, {
    type = erlang:error({required, type}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1155_TOC_PB_H).
-define(M_1155_TOC_PB_H, true).
-record(m_1155_toc, {
    type = erlang:error({required, type}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1156_TOC_PB_H).
-define(M_1156_TOC_PB_H, true).
-record(m_1156_toc, {
    type = erlang:error({required, type}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1157_TOS_PB_H).
-define(M_1157_TOS_PB_H, true).
-record(m_1157_tos, {
    
}).
-endif.

-ifndef(M_1157_TOC_PB_H).
-define(M_1157_TOC_PB_H, true).
-record(m_1157_toc, {
    list = []
}).
-endif.

-ifndef(P_WEEK_TASK_PB_H).
-define(P_WEEK_TASK_PB_H, true).
-record(p_week_task, {
    id = erlang:error({required, id}),
    value = erlang:error({required, value}),
    reward = erlang:error({required, reward})
}).
-endif.

-ifndef(M_1158_TOS_PB_H).
-define(M_1158_TOS_PB_H, true).
-record(m_1158_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1158_TOC_PB_H).
-define(M_1158_TOC_PB_H, true).
-record(m_1158_toc, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1159_TOS_PB_H).
-define(M_1159_TOS_PB_H, true).
-record(m_1159_tos, {
    
}).
-endif.

-ifndef(M_1159_TOC_PB_H).
-define(M_1159_TOC_PB_H, true).
-record(m_1159_toc, {
    day = erlang:error({required, day}),
    flag = erlang:error({required, flag})
}).
-endif.

-ifndef(M_1160_TOS_PB_H).
-define(M_1160_TOS_PB_H, true).
-record(m_1160_tos, {
    
}).
-endif.

-ifndef(M_1160_TOC_PB_H).
-define(M_1160_TOC_PB_H, true).
-record(m_1160_toc, {
    day = erlang:error({required, day})
}).
-endif.

-ifndef(M_1161_TOS_PB_H).
-define(M_1161_TOS_PB_H, true).
-record(m_1161_tos, {
    
}).
-endif.

-ifndef(M_1161_TOC_PB_H).
-define(M_1161_TOC_PB_H, true).
-record(m_1161_toc, {
    
}).
-endif.

-ifndef(M_1197_TOS_PB_H).
-define(M_1197_TOS_PB_H, true).
-record(m_1197_tos, {
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1197_TOC_PB_H).
-define(M_1197_TOC_PB_H, true).
-record(m_1197_toc, {
    
}).
-endif.

-ifndef(M_1198_TOS_PB_H).
-define(M_1198_TOS_PB_H, true).
-record(m_1198_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1198_TOC_PB_H).
-define(M_1198_TOC_PB_H, true).
-record(m_1198_toc, {
    
}).
-endif.

-ifndef(M_1199_TOS_PB_H).
-define(M_1199_TOS_PB_H, true).
-record(m_1199_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1199_TOC_PB_H).
-define(M_1199_TOC_PB_H, true).
-record(m_1199_toc, {
    
}).
-endif.

-ifndef(M_1201_TOS_PB_H).
-define(M_1201_TOS_PB_H, true).
-record(m_1201_tos, {
    type = erlang:error({required, type}),
    start = erlang:error({required, start}),
    num = erlang:error({required, num}),
    up_or_down = erlang:error({required, up_or_down}),
    date = erlang:error({required, date})
}).
-endif.

-ifndef(M_1201_TOC_PB_H).
-define(M_1201_TOC_PB_H, true).
-record(m_1201_toc, {
    list = [],
    num = erlang:error({required, num}),
    value = erlang:error({required, value})
}).
-endif.

-ifndef(P_RANK_INFO_PB_H).
-define(P_RANK_INFO_PB_H, true).
-record(p_rank_info, {
    num = erlang:error({required, num}),
    id = erlang:error({required, id}),
    role_id = erlang:error({required, role_id}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon}),
    vip = erlang:error({required, vip}),
    sign = erlang:error({required, sign}),
    value1 = erlang:error({required, value1}),
    value2,
    value3,
    value4,
    value5
}).
-endif.

-ifndef(M_1202_TOS_PB_H).
-define(M_1202_TOS_PB_H, true).
-record(m_1202_tos, {
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1202_TOC_PB_H).
-define(M_1202_TOC_PB_H, true).
-record(m_1202_toc, {
    list = [],
    type = erlang:error({required, type})
}).
-endif.

-ifndef(P_RANK_REWARD_PB_H).
-define(P_RANK_REWARD_PB_H, true).
-record(p_rank_reward, {
    num = erlang:error({required, num}),
    item_type = erlang:error({required, item_type}),
    item_num = erlang:error({required, item_num})
}).
-endif.

-ifndef(M_1301_TOS_PB_H).
-define(M_1301_TOS_PB_H, true).
-record(m_1301_tos, {
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1301_TOC_PB_H).
-define(M_1301_TOC_PB_H, true).
-record(m_1301_toc, {
    animals = [],
    role_list = [],
    type = erlang:error({required, type})
}).
-endif.

-ifndef(P_ANIMAL_PB_H).
-define(P_ANIMAL_PB_H, true).
-record(p_animal, {
    id = erlang:error({required, id}),
    base_id = erlang:error({required, base_id}),
    line_id = erlang:error({required, line_id}),
    point = erlang:error({required, point}),
    status = erlang:error({required, status}),
    red_bag = erlang:error({required, red_bag}),
    bomber_type,
    self_id,
    self_name
}).
-endif.

-ifndef(P_ANIMAL_ROLE_PB_H).
-define(P_ANIMAL_ROLE_PB_H, true).
-record(p_animal_role, {
    role_id = erlang:error({required, role_id}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon}),
    skill_list = [],
    vip = erlang:error({required, vip}),
    vip_effect = erlang:error({required, vip_effect})
}).
-endif.

-ifndef(P_SKILL_PB_H).
-define(P_SKILL_PB_H, true).
-record(p_skill, {
    type = erlang:error({required, type}),
    effect = erlang:error({required, effect}),
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1302_TOS_PB_H).
-define(M_1302_TOS_PB_H, true).
-record(m_1302_tos, {
    
}).
-endif.

-ifndef(M_1302_TOC_PB_H).
-define(M_1302_TOC_PB_H, true).
-record(m_1302_toc, {
    
}).
-endif.

-ifndef(M_1303_TOS_PB_H).
-define(M_1303_TOS_PB_H, true).
-record(m_1303_tos, {
    id = erlang:error({required, id}),
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(M_1303_TOC_PB_H).
-define(M_1303_TOC_PB_H, true).
-record(m_1303_toc, {
    
}).
-endif.

-ifndef(M_1304_TOS_PB_H).
-define(M_1304_TOS_PB_H, true).
-record(m_1304_tos, {
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1304_TOC_PB_H).
-define(M_1304_TOC_PB_H, true).
-record(m_1304_toc, {
    
}).
-endif.

-ifndef(M_1305_TOC_PB_H).
-define(M_1305_TOC_PB_H, true).
-record(m_1305_toc, {
    role_id = erlang:error({required, role_id}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1306_TOC_PB_H).
-define(M_1306_TOC_PB_H, true).
-record(m_1306_toc, {
    animals = []
}).
-endif.

-ifndef(M_1307_TOC_PB_H).
-define(M_1307_TOC_PB_H, true).
-record(m_1307_toc, {
    role = erlang:error({required, role})
}).
-endif.

-ifndef(M_1308_TOC_PB_H).
-define(M_1308_TOC_PB_H, true).
-record(m_1308_toc, {
    role_id = erlang:error({required, role_id})
}).
-endif.

-ifndef(M_1309_TOC_PB_H).
-define(M_1309_TOC_PB_H, true).
-record(m_1309_toc, {
    role_id = erlang:error({required, role_id}),
    type = erlang:error({required, type}),
    ids = []
}).
-endif.

-ifndef(P_ANIMAL_DIE_PB_H).
-define(P_ANIMAL_DIE_PB_H, true).
-record(p_animal_die, {
    id = erlang:error({required, id}),
    item_list = []
}).
-endif.

-ifndef(M_1310_TOC_PB_H).
-define(M_1310_TOC_PB_H, true).
-record(m_1310_toc, {
    base_id = erlang:error({required, base_id})
}).
-endif.

-ifndef(M_1311_TOC_PB_H).
-define(M_1311_TOC_PB_H, true).
-record(m_1311_toc, {
    id = []
}).
-endif.

-ifndef(M_1312_TOC_PB_H).
-define(M_1312_TOC_PB_H, true).
-record(m_1312_toc, {
    role_id = erlang:error({required, role_id}),
    type = erlang:error({required, type}),
    icon = erlang:error({required, icon}),
    effect
}).
-endif.

-ifndef(M_1313_TOC_PB_H).
-define(M_1313_TOC_PB_H, true).
-record(m_1313_toc, {
    list = []
}).
-endif.

-ifndef(P_ANIMAL_STATUS_PB_H).
-define(P_ANIMAL_STATUS_PB_H, true).
-record(p_animal_status, {
    id = erlang:error({required, id}),
    status = erlang:error({required, status})
}).
-endif.

-ifndef(M_1314_TOC_PB_H).
-define(M_1314_TOC_PB_H, true).
-record(m_1314_toc, {
    id = erlang:error({required, id}),
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1315_TOS_PB_H).
-define(M_1315_TOS_PB_H, true).
-record(m_1315_tos, {
    
}).
-endif.

-ifndef(M_1315_TOC_PB_H).
-define(M_1315_TOC_PB_H, true).
-record(m_1315_toc, {
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(M_1316_TOS_PB_H).
-define(M_1316_TOS_PB_H, true).
-record(m_1316_tos, {
    
}).
-endif.

-ifndef(M_1316_TOC_PB_H).
-define(M_1316_TOC_PB_H, true).
-record(m_1316_toc, {
    bonus = erlang:error({required, bonus}),
    num = erlang:error({required, num}),
    reward = erlang:error({required, reward})
}).
-endif.

-ifndef(M_1317_TOS_PB_H).
-define(M_1317_TOS_PB_H, true).
-record(m_1317_tos, {
    
}).
-endif.

-ifndef(M_1317_TOC_PB_H).
-define(M_1317_TOC_PB_H, true).
-record(m_1317_toc, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num}),
    reward = erlang:error({required, reward})
}).
-endif.

-ifndef(M_1318_TOS_PB_H).
-define(M_1318_TOS_PB_H, true).
-record(m_1318_tos, {
    
}).
-endif.

-ifndef(M_1318_TOC_PB_H).
-define(M_1318_TOC_PB_H, true).
-record(m_1318_toc, {
    animals = [],
    role_list = [],
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1319_TOS_PB_H).
-define(M_1319_TOS_PB_H, true).
-record(m_1319_tos, {
    
}).
-endif.

-ifndef(M_1319_TOC_PB_H).
-define(M_1319_TOC_PB_H, true).
-record(m_1319_toc, {
    role_id = erlang:error({required, role_id})
}).
-endif.

-ifndef(M_1320_TOC_PB_H).
-define(M_1320_TOC_PB_H, true).
-record(m_1320_toc, {
    role_id = erlang:error({required, role_id}),
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1321_TOS_PB_H).
-define(M_1321_TOS_PB_H, true).
-record(m_1321_tos, {
    type = erlang:error({required, type}),
    to_id = erlang:error({required, to_id})
}).
-endif.

-ifndef(M_1321_TOC_PB_H).
-define(M_1321_TOC_PB_H, true).
-record(m_1321_toc, {
    role_id = erlang:error({required, role_id}),
    type = erlang:error({required, type}),
    to_id = erlang:error({required, to_id})
}).
-endif.

-ifndef(M_1401_TOS_PB_H).
-define(M_1401_TOS_PB_H, true).
-record(m_1401_tos, {
    
}).
-endif.

-ifndef(M_1401_TOC_PB_H).
-define(M_1401_TOC_PB_H, true).
-record(m_1401_toc, {
    list = []
}).
-endif.

-ifndef(P_MAIL_PB_H).
-define(P_MAIL_PB_H, true).
-record(p_mail, {
    id = erlang:error({required, id}),
    title = erlang:error({required, title}),
    msg = erlang:error({required, msg}),
    items = [],
    time = erlang:error({required, time}),
    timeout = erlang:error({required, timeout}),
    status = erlang:error({required, status})
}).
-endif.

-ifndef(M_1402_TOS_PB_H).
-define(M_1402_TOS_PB_H, true).
-record(m_1402_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1402_TOC_PB_H).
-define(M_1402_TOC_PB_H, true).
-record(m_1402_toc, {
    list = []
}).
-endif.

-ifndef(M_1403_TOS_PB_H).
-define(M_1403_TOS_PB_H, true).
-record(m_1403_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1403_TOC_PB_H).
-define(M_1403_TOC_PB_H, true).
-record(m_1403_toc, {
    
}).
-endif.

-ifndef(M_1404_TOS_PB_H).
-define(M_1404_TOS_PB_H, true).
-record(m_1404_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1404_TOC_PB_H).
-define(M_1404_TOC_PB_H, true).
-record(m_1404_toc, {
    
}).
-endif.

-ifndef(M_1405_TOS_PB_H).
-define(M_1405_TOS_PB_H, true).
-record(m_1405_tos, {
    
}).
-endif.

-ifndef(M_1405_TOC_PB_H).
-define(M_1405_TOC_PB_H, true).
-record(m_1405_toc, {
    list = []
}).
-endif.

-ifndef(M_1406_TOS_PB_H).
-define(M_1406_TOS_PB_H, true).
-record(m_1406_tos, {
    
}).
-endif.

-ifndef(M_1406_TOC_PB_H).
-define(M_1406_TOC_PB_H, true).
-record(m_1406_toc, {
    
}).
-endif.

-ifndef(M_1407_TOC_PB_H).
-define(M_1407_TOC_PB_H, true).
-record(m_1407_toc, {
    
}).
-endif.

-ifndef(M_1601_TOS_PB_H).
-define(M_1601_TOS_PB_H, true).
-record(m_1601_tos, {
    
}).
-endif.

-ifndef(M_1601_TOC_PB_H).
-define(M_1601_TOC_PB_H, true).
-record(m_1601_toc, {
    start_time = erlang:error({required, start_time}),
    end_time = erlang:error({required, end_time}),
    status = erlang:error({required, status}),
    num = erlang:error({required, num}),
    all_num = erlang:error({required, all_num})
}).
-endif.

-ifndef(M_1602_TOS_PB_H).
-define(M_1602_TOS_PB_H, true).
-record(m_1602_tos, {
    
}).
-endif.

-ifndef(M_1602_TOC_PB_H).
-define(M_1602_TOC_PB_H, true).
-record(m_1602_toc, {
    list = []
}).
-endif.

-ifndef(P_SHOP_ITEM_PB_H).
-define(P_SHOP_ITEM_PB_H, true).
-record(p_shop_item, {
    id = erlang:error({required, id}),
    type = erlang:error({required, type}),
    price = erlang:error({required, price}),
    need_num = erlang:error({required, need_num})
}).
-endif.

-ifndef(M_1603_TOC_PB_H).
-define(M_1603_TOC_PB_H, true).
-record(m_1603_toc, {
    status = erlang:error({required, status})
}).
-endif.

-ifndef(M_1605_TOS_PB_H).
-define(M_1605_TOS_PB_H, true).
-record(m_1605_tos, {
    id = erlang:error({required, id}),
    phone
}).
-endif.

-ifndef(M_1605_TOC_PB_H).
-define(M_1605_TOC_PB_H, true).
-record(m_1605_toc, {
    
}).
-endif.

-ifndef(M_1701_TOS_PB_H).
-define(M_1701_TOS_PB_H, true).
-record(m_1701_tos, {
    
}).
-endif.

-ifndef(M_1701_TOC_PB_H).
-define(M_1701_TOC_PB_H, true).
-record(m_1701_toc, {
    list = [],
    time = erlang:error({required, time}),
    reward = []
}).
-endif.

-ifndef(M_1702_TOC_PB_H).
-define(M_1702_TOC_PB_H, true).
-record(m_1702_toc, {
    list = [],
    time = erlang:error({required, time}),
    reward = []
}).
-endif.

-ifndef(P_PROCESS_PB_H).
-define(P_PROCESS_PB_H, true).
-record(p_process, {
    id = erlang:error({required, id}),
    type = erlang:error({required, type}),
    value = erlang:error({required, value}),
    target = erlang:error({required, target})
}).
-endif.

-ifndef(M_1703_TOC_PB_H).
-define(M_1703_TOC_PB_H, true).
-record(m_1703_toc, {
    list = []
}).
-endif.

-ifndef(M_1704_TOS_PB_H).
-define(M_1704_TOS_PB_H, true).
-record(m_1704_tos, {
    
}).
-endif.

-ifndef(M_1704_TOC_PB_H).
-define(M_1704_TOC_PB_H, true).
-record(m_1704_toc, {
    id = erlang:error({required, id}),
    value = erlang:error({required, value}),
    target = erlang:error({required, target})
}).
-endif.

-ifndef(M_1705_TOC_PB_H).
-define(M_1705_TOC_PB_H, true).
-record(m_1705_toc, {
    id = erlang:error({required, id}),
    value = erlang:error({required, value}),
    target = erlang:error({required, target})
}).
-endif.

-ifndef(M_1706_TOC_PB_H).
-define(M_1706_TOC_PB_H, true).
-record(m_1706_toc, {
    value = erlang:error({required, value})
}).
-endif.

-ifndef(M_1707_TOS_PB_H).
-define(M_1707_TOS_PB_H, true).
-record(m_1707_tos, {
    
}).
-endif.

-ifndef(M_1707_TOC_PB_H).
-define(M_1707_TOC_PB_H, true).
-record(m_1707_toc, {
    list = []
}).
-endif.

-ifndef(P_DAILY_TASK_PB_H).
-define(P_DAILY_TASK_PB_H, true).
-record(p_daily_task, {
    id = erlang:error({required, id}),
    value = erlang:error({required, value})
}).
-endif.

-ifndef(M_1708_TOC_PB_H).
-define(M_1708_TOC_PB_H, true).
-record(m_1708_toc, {
    task = erlang:error({required, task})
}).
-endif.

-ifndef(M_1709_TOC_PB_H).
-define(M_1709_TOC_PB_H, true).
-record(m_1709_toc, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1801_TOS_PB_H).
-define(M_1801_TOS_PB_H, true).
-record(m_1801_tos, {
    
}).
-endif.

-ifndef(M_1801_TOC_PB_H).
-define(M_1801_TOC_PB_H, true).
-record(m_1801_toc, {
    steps = []
}).
-endif.

-ifndef(M_1802_TOS_PB_H).
-define(M_1802_TOS_PB_H, true).
-record(m_1802_tos, {
    step = erlang:error({required, step})
}).
-endif.

-ifndef(M_1802_TOC_PB_H).
-define(M_1802_TOC_PB_H, true).
-record(m_1802_toc, {
    
}).
-endif.

-ifndef(M_1803_TOS_PB_H).
-define(M_1803_TOS_PB_H, true).
-record(m_1803_tos, {
    
}).
-endif.

-ifndef(M_1803_TOC_PB_H).
-define(M_1803_TOC_PB_H, true).
-record(m_1803_toc, {
    flag = erlang:error({required, flag}),
    day
}).
-endif.

-ifndef(M_1804_TOS_PB_H).
-define(M_1804_TOS_PB_H, true).
-record(m_1804_tos, {
    
}).
-endif.

-ifndef(M_1804_TOC_PB_H).
-define(M_1804_TOC_PB_H, true).
-record(m_1804_toc, {
    
}).
-endif.

-ifndef(M_1901_TOS_PB_H).
-define(M_1901_TOS_PB_H, true).
-record(m_1901_tos, {
    
}).
-endif.

-ifndef(M_1901_TOC_PB_H).
-define(M_1901_TOC_PB_H, true).
-record(m_1901_toc, {
    num = erlang:error({required, num}),
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1902_TOS_PB_H).
-define(M_1902_TOS_PB_H, true).
-record(m_1902_tos, {
    
}).
-endif.

-ifndef(M_1902_TOC_PB_H).
-define(M_1902_TOC_PB_H, true).
-record(m_1902_toc, {
    coin = erlang:error({required, coin})
}).
-endif.

-ifndef(P_ACTIVE_PB_H).
-define(P_ACTIVE_PB_H, true).
-record(p_active, {
    type = erlang:error({required, type}),
    start_time = erlang:error({required, start_time}),
    end_time = erlang:error({required, end_time})
}).
-endif.

-ifndef(M_1903_TOS_PB_H).
-define(M_1903_TOS_PB_H, true).
-record(m_1903_tos, {
    
}).
-endif.

-ifndef(M_1903_TOC_PB_H).
-define(M_1903_TOC_PB_H, true).
-record(m_1903_toc, {
    list = []
}).
-endif.

-ifndef(M_1904_TOS_PB_H).
-define(M_1904_TOS_PB_H, true).
-record(m_1904_tos, {
    
}).
-endif.

-ifndef(M_1904_TOC_PB_H).
-define(M_1904_TOC_PB_H, true).
-record(m_1904_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1905_TOS_PB_H).
-define(M_1905_TOS_PB_H, true).
-record(m_1905_tos, {
    
}).
-endif.

-ifndef(M_1905_TOC_PB_H).
-define(M_1905_TOC_PB_H, true).
-record(m_1905_toc, {
    num = erlang:error({required, num})
}).
-endif.

