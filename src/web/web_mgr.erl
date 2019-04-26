%%----------------------------------------------------
%%  回调地址 基于mochiweb
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web_mgr).
-export([start_link/0
        ,stop/0
    ]).
-include("common.hrl").



start_link() ->
    Port = sys_env:get_env(web_port),
    ?ERR("后台成功监听~w端口", [Port]),
	mochiweb_http:start([{port, Port},{ssl, false},
		{loop, fun dispatch_requests/1}]).


stop() ->
  mochiweb_http:stop().

dispatch_requests(Req) ->	
	Path = Req:get(path),
	Socket = Req:get(socket),
	Action = clean_path(Path),
	web:handle(Action, Req, Socket).

clean_path(Path) ->
	case string:str(Path, "?") of
		0 ->
			Path;
		N ->
			string:substr(Path, 1, string:len(Path) - (N + 1))
	end.
