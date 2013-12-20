%% Author: Administrator
%% Created: 2012-3-12
%% Description: TODO: Add description to url_codec
-module(url_codec).

%%
%% Include files
%%
-include("url_codec.hrl").

%%
%% Exported Functions
%%
-export([decode/1]).

%%
%% API Functions
%%
-spec decode(UrlString::binary()|string()) ->
		  {ok, Url::#url{}}.
decode(UrlString) when is_list(UrlString) ->
	parse_params(UrlString);
decode(UrlString) when is_binary(UrlString) ->
	decode(binary_to_list(UrlString)).

%%
%% Local Functions
%%
-spec parse_params(UrlStrintg::string()) ->
		  {ok, Url::#url{}}.
parse_params(UrlString) ->
	case re:split(UrlString, "[?]", [{return, list}]) of
		[H, ParamsString] ->
			Params = lists:map(fun(X) -> [Key, Value] = re:split(X, "[=]", [{return, list}]), 
										 {Key, escape_codec:decode(Value)} 
							   end,
							   re:split(ParamsString, "[&]", [{return, list}])),
			{ok, Ret} = parse_protocol(H),
			{ok, Ret#url{params=Params}};
		[UrlString] ->
			parse_protocol(UrlString)
	end.
				
-spec parse_protocol(UrlStrintg::string()) ->
		  {ok, Url::#url{}}.
parse_protocol(UrlString) ->
	case re:split(UrlString, "://", [{return, list}]) of
		[Protocol, Rest] ->
			{ok, Ret} = parse_host(Rest),
			{ok, Ret#url{protocol=Protocol}};
		[Path] ->
			{ok, #url{path=Path}}
	end.
				
-spec parse_host(UrlStrintg::string()) ->
		  {ok, Url::#url{}}.
parse_host(UrlString) ->
	{match, [HostPort, Path]} = re:run(UrlString, "([^/]*)(/.*)", [{capture, all_but_first, list}]),
	case re:split(HostPort, ":", [{return, list}]) of
		[Host, PortString] ->
			{ok, #url{path=Path,host=Host,port=list_to_integer(PortString)}};
		[Host] ->
			{ok, #url{path=Path,host=Host}}
	end.
