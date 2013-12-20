%% Author: liushi
%% Created: May 21, 2012
%% Description: TODO: Add description to rtp_server
-module(rtp_server).

%%
%% Include files
%%
-include("rtp_session.hrl").
-include("rtsp.hrl").
-include("log.hrl").
%%
%% Exported Functions
%%
-export([start_link/1,get_sdp/2, setup/2, play/2, pause/2, close/1,teardown/1,set_parameter/2]).

%% -record(rtp_server_route, {key = default :: string(),
%% 						   module :: module()}).

%%
%% API Functions
%%
start_link(URL) ->
	{ok, Module} = get_mod(URL),
	Module:start_link().

-spec get_sdp(Pid::pid(),URL::string()) -> {ok, SDP::string(),SessionId::string()} | {error,Reason::atom()}.			  
get_sdp(Pid,URL) ->
	{ok, Module} = get_mod(URL),
	Module:get_sdp(Pid,URL).

-spec setup(Pid::pid(),Setup::#setup{}) -> {ok, {Pid::pid(),ServerPort::{0..65536,0..65536}}} | {error,Error::atom()}.
%% setup(undefined,#setup{url=URL}=Setup) ->	
%% 	{ok, Module} = get_mod(URL),
%% 	{ok,Pid} = Module:start_link(),
%% 	setup(Pid,Setup);
setup(Pid,#setup{url=URL}=Setup) ->	
	{ok, Module} = get_mod(URL),
	case Module:setup(Pid, Setup) of
		%% udp
		{ok,ServerPorts} -> {ok,ServerPorts};
		E ->
			E
	end.
	
-spec play(Pid::pid(), Play::#play{}) -> ok.
play(Pid, Play) ->
	gen_server:call(Pid, Play,infinity).

-spec pause(Pid::pid(), Pause::#pause{}) -> ok.
pause(Pid, Pause) ->
	gen_server:call(Pid, Pause,infinity).


set_parameter(Pid, Parameters) ->
	gen_server:call(Pid, {set_parameter,Parameters},infinity).

-spec close(Pid::port()) -> ok.
close(Pid) ->
	gen_server:call(Pid, close).	

-spec teardown(Pid::port()) -> ok.
teardown(Pid) ->
	gen_server:call(Pid, teardown).	

%%
%% Local Functions
%%
-spec get_mod(URL::string()|binary()) -> {ok, Module::module()}.
get_mod(URL) ->
	{ok, #url{path=Path}} = url_codec:decode(URL),
	{match, [Key]} = re:run(Path, "/([^/]*)", [{capture, [1], list}]),
%% 	?DEBUG("~p -- get mod of url:~p~n",[?MODULE,{URL,Key}]),
	Module = proplists:get_value(Key, ?RTSP_SERVER_ROUTE_MOD_MAPPING, ?DEFAULT_RTSP_SERVER_MOD),
	{ok, Module}.
	
	
	
	
		  
