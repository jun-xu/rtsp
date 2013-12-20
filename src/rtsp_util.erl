%% Author: sunshine
%% Created: 2013-4-23
%% Description: TODO: Add description to rtsp_util
-module(rtsp_util).

%%
%% Include files
%%
-include("rtsp.hrl").
%%
%% Exported Functions
%%
-export([get_self_ipv4/0,now_in_millisecond/0,get_track/1,get_app_env/3,open_groupsocket/1,choose_file_reader/1]).

%%
%% API Functions
%%
get_self_ipv4() ->
	{ok,[{Addr,_,_}|_T]} = inet:getif(),
	inet_parse:ntoa(Addr).
	
choose_file_reader(FilePath) ->
	Extends = filename:extension(FilePath),
	proplists:get_value(Extends, ?RTSP_FILE_MOD_MAPPING).

-spec now_in_millisecond() -> integer().
now_in_millisecond() ->
	{A,S,M} = now(),
	A * 1000000000 + S * 1000 + M div 1000.

%% track 1: video
%% track 2: audio
-spec get_track(URL::string()) -> {ok, Track::integer()} | error.
get_track(URL) ->
	case re:run(URL, "track([0-9]*)$", [{capture, all_but_first, list}, caseless]) of
		{match, [Track]} ->
			{ok, list_to_integer(Track)};
		_ ->
			error
	end.

-spec get_app_env(Key::atom(),Opts::[tuple()],Default::any()) -> any().
get_app_env(Key, Opts, Default) ->
    case proplists:get_value(Key, Opts) of
        undefined ->
					case application:get_env(?APP_NAME,Key) of
                		{ok, Value} -> Value;
                		undefined ->  Default
            		end;
        Value ->
            Value
    end.

-spec open_groupsocket(Pid::pid()) ->
		  {ok, {{RtpPort::integer(),RtcpPort::integer()},{RtpSocket::port(),RtcpSocket::port()}}} | error.
open_groupsocket(Pid) ->
	random:seed(now()),
	Port = 2 * random:uniform(20000) + 10000,
	loop_open_groupsocket(Pid,Port).

loop_open_groupsocket(_Pid,Port) when Port > 65536 -> error;
loop_open_groupsocket(Pid,Port) ->
	case {gen_udp:open(Port, [{active, true}, binary,{sndbuf, ?SND_BUF_SIZE}]),
		  gen_udp:open(Port+1, [{active, true}, binary])} of
		{{ok, RtpSocket}, {ok, RtcpSocket}} ->
			gen_udp:controlling_process(RtpSocket, Pid),
			gen_udp:controlling_process(RtcpSocket, Pid),
			{ok, {{Port, Port+1}, {RtpSocket, RtcpSocket}}};
		{{ok, RtpSocket}, _} ->
			gen_udp:close(RtpSocket),
			timer:sleep(50),
			loop_open_groupsocket(Pid,Port+2);
		{_, {ok, RtcpSocket}} ->
			gen_udp:close(RtcpSocket),
			timer:sleep(50),
			loop_open_groupsocket(Pid,Port+2);
		_ ->
			timer:sleep(50),
			loop_open_groupsocket(Pid,Port+2)
	end.
%%
%% Local Functions
%%

