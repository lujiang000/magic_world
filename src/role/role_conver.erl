%%----------------------------------------------------
%% @doc 人物数据转换
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_conver).
-export([
        to_online/1
        ,to_login_role/1
        ,to_animal_role/1
        ,to_login_role/2
    ]).

-include("role.hrl").
-include("animal.hrl").
-include("all_pb.hrl").

%% 转换在线数据
to_online(#role{role_id = RoleID, name = Name, icon = Icon, pid = Pid, socket_pid = SocketPid, gold = Gold, coin = Coin, vip = Vip, screat = Screat, sign = Sign}) ->
    #online_role{role_id = RoleID, name = Name, pid = Pid, socket_pid = SocketPid, icon = Icon, gold = Gold, coin = Coin, vip = Vip, screat = Screat, sign = Sign}.

%% 登陆需要数据
to_login_role(#role{role_id = RoleID, open_id = OpenID, name = Name, icon = Icon,  vip = Vip, gold = Gold, coin = Coin, status = Status, use_coin = UseCoin, first_gift = Flag, id_card = Card, sign = Sign, phone = Phone, channel = Channel, vip_effect = VipEffect, subscribe = Sub, subscribe_reward = SubReward}) ->
  Card1 = case Card =:= "" of
      true -> 0;
      _ -> 1
  end,
  Subscribe = case SubReward of
      1 -> 2;
      _ -> Sub
  end,
  #p_role_info{role_id = RoleID, nick_name = Name, openid = OpenID, icon = Icon,  vip = Vip, gold = Gold, coin = Coin, status = Status, use_coin = UseCoin, first_gift = Flag, id_card = Card1, sign = Sign, phone = Phone, channel = Channel, vip_effect = VipEffect, subscribe = Subscribe}.

to_login_role(#role{role_id = RoleID, open_id = OpenID, name = Name, icon = Icon,  vip = Vip, gold = Gold, coin = Coin, status = Status, use_coin = UseCoin, first_gift = Flag, id_card = Card, sign = Sign, phone = Phone, channel = Channel, vip_effect = VipEffect, subscribe = Sub, subscribe_reward = SubReward}, regist) ->
  Card1 = case Card =:= "" of
      true -> 0;
      _ -> 1
  end,
  Subscribe = case SubReward of
      1 -> 2;
      _ -> Sub
  end,
  #p_role_info{role_id = RoleID, nick_name = Name, openid = OpenID, icon = Icon,  vip = Vip, gold = Gold, coin = Coin, status = Status, use_coin = UseCoin, first_gift = Flag, id_card = Card1, sign = Sign, phone = Phone, channel = Channel, regist = 1, vip_effect = VipEffect, subscribe = Subscribe}.

%% 转换动物园数据
to_animal_role(#role{role_id = RoleID, name = Name, icon = Icon, pid = Pid, socket_pid = SocketPid, vip = Vip, skill_list = SkillList, vip_effect = VipEffect}) ->
    #animal_role{role_id = RoleID, name = Name, icon = Icon, pid = Pid, socket_pid = SocketPid, vip = Vip, skill_id = SkillList, vip_effect = VipEffect}.



