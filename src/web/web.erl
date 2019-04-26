%%----------------------------------------------------
%% @doc 所有回调接口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web).
-export([
        handle/3
    ]).

-include("common.hrl").
-define(OK, <<"ok">>).
-define(SUCCESS, <<"success">>).
-define(FAIL, <<"fail">>).
-define(Error, <<"server error">>).
-define(true, <<"true">>).
-define(false, <<"false">>).


%% 所有回调接受信息并且返回
%% 后台 parse_qs() get请求  parse_post() post请求
handle("/web_rpc", Req, Socket) ->
    {ok, {Ip, _Port}} = inet:peername(Socket),
    List = sys_env:get_env(web_ip),
    case catch lists:member(Ip, List) of
        true ->
            Params = Req:parse_qs(),
            case catch web_callback:do_rpc(Params) of
                true ->
                    success(Req, ?true);
                false ->
                    send_error(Req, ?false);
                Data ->
                    success(Req, Data)
            end;
        _ ->
            send_error(Req, ?false)
    end;


handle("/wft", Req, _) ->
  Params = Req:parse_post(),
  Reply = web_callback:wft(Params),
  success(Req, Reply);

handle("/yao_zfb", Req, _) ->
  Params = Req:parse_post(),
  Reply = web_callback:yao_zfb(Params),
  success(Req, Reply);

handle("/yb", Req, _) ->
  Params = Req:parse_post(),
  Reply = web_callback:yb_pay(Params),
  case Reply of
      <<"fail">> ->
          ?ERR("收到数据:~w", [Params]);
      _ ->
          ok
  end,
  success(Req, Reply);

handle("/paysapi", Req, _) ->
  Params = case catch Req:parse_post() of
       _A -> _A
   end,
  case catch web_callback:paysapi(Params) of
      <<"success">> ->
          success(Req, ?SUCCESS);
      _ ->
          fail(Req, ?FAIL)
 end;


handle("/再见", Req, _) ->
  _Params = Req:parse_post(),
  fail(Req, <<"你好">>);

handle(Unknown, Req, _) ->
	Req:respond({404, [{"Content-Type", "text/plain"}], subst("Unknown action: ~s", [Unknown])}).


fail(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "charset=utf-8"},
    {"Content-Language", "en,zh"}], Body}).

send_error(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).
