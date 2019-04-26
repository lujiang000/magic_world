%%%-------------------------------------------------------------------
%%% @author zc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2017 上午9:57
%%%-------------------------------------------------------------------
-module(weixin_mgr).
-author("zc").

-behaviour(gen_server).
-include("common.hrl").

%% API
-export([
  start_link/0
  ,client_sign/1
  ,get_access_token/0
  ,reload_access_token/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  access_token = ""   %% 获取到的凭证
  ,jsapi_ticket = ""  %%
  ,expires_in = 0     %% 凭证有效时间，单位：秒
  ,end_time = 0       %% 结束时刻
  ,time_ref           %% 定时器 定时获取access_token 和 jsapi_ticket
  ,error_times = 0    %% 获取access_token超过10次报警
}).

-define(ticket_timeout, 7000000).  %% 过期时间


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% 前端分享签名
client_sign(Url) ->
  catch gen_server:call(?MODULE, {sign, Url}).

get_access_token() ->
  catch gen_server:call(?MODULE, get_access_token).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  ?INFO("[~w] 正在启动", [?MODULE]),
  erlang:process_flag(trap_exit, true),
  case sys_env:get_env(ip) of
      "127.0.0.1" -> ok;
      "120.76.28.170" -> ok;
      _ ->
          self() ! reload_access_token_jsapi_ticket
  end,
  ?INFO("[~w] 启动完成", [?MODULE]),
  {ok, #state{}}.

handle_call({sign, Url}, _From, State = #state{jsapi_ticket = JsApiTicket}) ->
  TimeStamp = date:unixtime(),
  NonceStr = sys_rand:rand(100000),
  Sign = sign(Url, JsApiTicket, TimeStamp, NonceStr),
  {reply, {ok, Sign, TimeStamp, NonceStr}, State};


handle_call(get_access_token, _From, State = #state{access_token = AT, jsapi_ticket = JaTk}) ->
  {reply, {ok, {AT, JaTk}}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.




handle_cast(_Request, State) ->
    {noreply, State}.

%%重载access_token
handle_info(reload_access_token_jsapi_ticket, State = #state{time_ref = Ref, error_times = Times}) ->
    NewState = case reload_access_token() of
        {ok, ACToken, Expires, JsApiTicket} ->
            case erlang:is_reference(Ref) of
                true ->
                    erlang:cancel_timer(Ref);
                _ -> 
                    ok
            end,
            Ref1 = erlang:send_after(?ticket_timeout, self(), reload_access_token_jsapi_ticket),
            State#state{access_token = ACToken, expires_in = Expires, time_ref = Ref1, jsapi_ticket = JsApiTicket, error_times = 0};
        {ok, ACToken, Expires} ->
            erlang:send_after(1000, self(), reload_jsapi_ticket),
            State#state{access_token = ACToken, expires_in = Expires, error_times = Times + 1};
        _ ->
            case Times >= 10 of
                true ->
                    ?ERR("获取access_token错误超过10次停止请求", []);
                _ ->
                    erlang:send_after(1000, self(), reload_access_token_jsapi_ticket),
                    State#state{error_times = Times + 1}
            end
    end,
    {noreply, NewState};

%%重载ticket
handle_info(reload_jsapi_ticket, State = #state{access_token = ACToken, time_ref = Ref, error_times = Times}) ->
    NewState = case get_api_ticket(ACToken) of
        {ok, JsApiTicket} ->
            case erlang:is_reference(Ref) of
                true ->
                    erlang:cancel_timer(Ref);
                _ -> 
                    ok
            end,
            Ref1 = erlang:send_after(?ticket_timeout, self(), reload_access_token_jsapi_ticket),
            State#state{jsapi_ticket = JsApiTicket, time_ref = Ref1, error_times = 0};
        _ ->
            case Times >= 10 of
                true ->
                    ?ERR("获取ticket错误超过10次停止请求", []);
                _ ->
                    erlang:send_after(1000, self(), reload_jsapi_ticket),
                    State#state{error_times = Times + 1}
            end
    end,
    {noreply, NewState};



handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


reload_access_token() ->
  PropList = [
    {grant_type, client_credential},
    {appid, sys_env:get_env(loginAppId)},
    {secret, sys_env:get_env(loginAppSecret)}
  ],
  Params1 = util:format_get_params(PropList),
  Arg = "https://api.weixin.qq.com/cgi-bin/token" ++ "?" ++ Params1,
  Headers = [{"content-type", "ext/xml;charset=utf-8"}],
  case catch httpc:request(get, {Arg, Headers}, [{timeout, 5000}], []) of
    {error,{failed_connect,[{to_address,{"api.weixin.qq.com",
      443}},
      {inet,[inet],nxdomain}]}} ->
      {error, 0};
    {ok, {Header, _List, Result}} ->
      case Header of
        {_, 200, _} ->
          case catch json:decode(erlang:list_to_binary(Result), [{object_format, proplist}]) of
            Params when is_list(Params)->
              case lists:keyfind(<<"access_token">>, 1, Params) of
                {_, ACToken} ->
                  case lists:keyfind(<<"expires_in">>, 1, Params) of
                    {_, Expires} ->
                      case get_api_ticket(ACToken) of
                        {ok, JsApiTicket} ->
                          {ok, ACToken, Expires, JsApiTicket};
                        _ ->
                          {ok, ACToken, Expires}
                      end;
                    _ ->
                      {error, 1}
                  end;
                _ ->
                  case lists:keyfind(<<"errcode">>, 1, Params) of
                    {_, _ErrorCode} ->
                      ok;
%%                      ?ERR("ErrorCode:~p", [ErrorCode]);
                    _ ->
                      ok
                  end,
                  {error, 1}
              end;
            _ ->
              {error, 1}
          end;
        _ ->
          {error, 1}
      end;
    {error,socket_closed_remotely} ->
      {error, 1};
    _ ->
      {error, 1}
  end.


get_api_ticket(ACToken) ->
  P = [
    {access_token, ACToken},
    {type, jsapi}
  ],
  Url = "https://api.weixin.qq.com/cgi-bin/ticket/getticket",
  Headers = [{"content-type", "ext/xml;charset=utf-8"}],
  Params1 = util:format_get_params(P),
  Arg = Url ++ "?" ++ Params1,
  case catch httpc:request(get, {Arg, Headers}, [{timeout, 5000}], []) of
    {ok, {H, _, R}} ->
      case H of
        {_, 200, _} ->
          case json:decode(erlang:list_to_binary(R), [{object_format, proplist}]) of
            Params when is_list(Params)->
              case lists:keyfind(<<"ticket">>, 1, Params) of
                {_, APiTK} ->
                  {ok, APiTK};
                _ ->
                  {error, 1}
              end;
            _ ->
              {error, 1}
          end;
        _ ->
          {error, 1}
      end;
    _ ->
      {error, 1}
  end.

sign(Url, ApiTicket, TimeStamp, NonceStr) ->
  PA = [
    {'jsapi_ticket', ApiTicket},
    {'noncestr', NonceStr},
    {'timestamp', TimeStamp},
    {'url', Url}
  ],
  String1 = util:format_get_params(PA),
  Sha = crypto:hash(sha, String1),
  Sign = util:to_16(Sha),
%%
%%
%%  ?ERR("S1:~p", [Sign]),
%%
%%  List = lists:map(fun(X)->
%%    [hex(X div 16),hex(X rem 16)] end,binary_to_list(Sha)),
%%  S2 = lists:flatten(List),
%%  ?ERR("S2:~p", [S2]),
  Sign.

