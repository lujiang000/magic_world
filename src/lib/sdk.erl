%%%-------------------------------------------------------------------
%%% @author weichengjun
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jan 2016 2:09 PM
%%%-------------------------------------------------------------------
-module(sdk).

%% API
-export([http/3]).

-export([http_post/4, http_post/5]).

-export([format_http_get_params/1, format_http_post_params/1]).

-export([format_xml_params/1
        ,format_xml_params/2
    ]).

-export([format_val/1, format_http_key_val/1, order_by_ascii_for_http_get/1, order_by_ascii_for_http_post/1]).

http(Method, Url, Params) ->
    case http_request(Method, Url, Params) of
        {ok, {{_Version, _, _ReasonPhrase}, _ReturnHeaders, Result}} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

format_http_key_val(String) ->
    List = string:tokens(String, "&"),
    [list_to_tuple(string:tokens(H, "=")) || H <- List].
%%
order_by_ascii_for_http_get(List) ->
    L = [format_key(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- List],
    LL = lists:sort(L),
    string:join([H || H <- LL], "&").

order_by_ascii_for_http_post(List) ->
    L = [format_key(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- List],
    LL = lists:sort(L),
    string:join([H || H <- LL], ";").
format_http_get_params(PropList) ->
    string:join([format_key(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- PropList], "&").

format_http_post_params(PropList) ->
    string:join([format_key(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- PropList], ";").

format_xml_params(PropList) ->
    Body = string:join(["<" ++ atom_to_list(Key) ++ "><![CDATA[" ++ format_val(Val) ++ "]]></" ++ atom_to_list(Key) ++ ">"
        || {Key, Val} <- PropList], "\n"),
    "<xml>\n"++Body++"\n</xml>".

format_xml_params(PropList, SignList) ->
    Body = string:join(["<" ++ atom_to_list(Key) ++ "><![CDATA[" ++ format_val(Val) ++ "]]></" ++ atom_to_list(Key) ++ ">"
        || {Key, Val} <- PropList], "\n"),
    Sign =  string:join(["<" ++ atom_to_list(Key) ++ ">" ++ format_val(Val) ++ "</" ++ atom_to_list(Key) ++ ">"
        || {Key, Val} <- SignList], "\n"),
    "<xml>\n"++Body++ "\n" ++Sign++"\n</xml>".

format_key(Key) when is_atom(Key) ->
    atom_to_list(Key);
format_key(Key) -> Key.

%% Internal
http_request(Method, Url, Params) ->
    case Method of
        get ->
            Arg = Url ++ "?" ++ Params,
            http_get(Arg);
        post ->
            http_post(Url, Params)
    end.

http_get(Arg) ->
    Headers = [
        {"charset", "utf-8"},
        {"content-type", "text/html"}],
    httpc:request(get, {Arg, Headers}, [{timeout, 5000}], []).

http_post(Url, Body) ->
    Headers = [{"content-type", "application/json;charset=utf-8"}],
    ContentType = "application/x-www-form-urlencoded",
    httpc:request(post, {Url, Headers, ContentType, Body}, [{timeout, 5000}], []).

http_post(Url, Headers, ContentType, Body) ->
    httpc:request(post, {Url, Headers, ContentType, Body}, [{timeout, 5000}], []).

http_post(Url, Headers, ContentType, Body, HttpOps) ->
    httpc:request(post, {Url, Headers, ContentType, Body}, HttpOps, []).

format_val(Val) when is_list(Val) ->
    Val;
format_val(Val) when is_binary(Val) ->
    binary_to_list(Val);
format_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
format_val(Val) when is_float(Val) ->
    float_to_list(Val).

