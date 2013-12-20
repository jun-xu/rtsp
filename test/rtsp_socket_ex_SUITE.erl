%% Author: sunshine
%% Created: 2013-12-17
%% Description: TODO: Add description to rtsp_socket_ex_SUITE
-module(rtsp_socket_ex_SUITE).

%%
%% Include files
%%
-include("rtp_session.hrl").
-include("rtsp.hrl").
-include("log.hrl").
%%
%% Exported Functions
%%
-export([]).

-compile(export_all).

%%
%% API Functions
%%
suite() ->
	[].

init_per_suite(Config) ->
	Config.

end_per_suite(_Config) ->
	ok.

init_per_testcase(_TestCase, Config) ->
	Config.
end_per_testcase(_TestCase, _Config) ->
	ok.

all() ->
	[
	
	test_common
	 ].

test_common(_) ->
	%% first options.
	rtsp_client_sup:start_link(),
	root_dir_manager:start_link(),
	root_dir_manager:add_root_dir("../../test"),
	{ok,_} = rtsp_server_listener:start_link(65000),
	{ok,Socket} = gen_tcp:connect({127,0,0,1},65000,[binary,{active,false}],1000),
	Options = <<"OPTIONS rtsp://127.0.0.1:56000/files/9min.avi RTSP/1.0\r\n",
				"CSeq: 2\r\n",
				"User-Agent: LibVLC/2.1.2 (LIVE555 Streaming Media v2013.12.05)\r\n\r\n">>,
	ok = gen_tcp:send(Socket, Options),
	OptionsReps = <<"RTSP/1.0 200 OK\r\n",
			  "CSeq: 2\r\n",
			  ?RTSP_OPTIONS_SUPPORT_FUNCTION, 
		      "\r\n">>,
	{ok,OptionsReps} = gen_tcp:recv(Socket, 0),
	%% second, discribe.
	Discribe = <<"DESCRIBE rtsp://127.0.0.1:56000/files/9min.avi RTSP/1.0\r\n",
				 "CSeq: 3\r\n",
				 "User-Agent: LibVLC/2.1.2 (LIVE555 Streaming Media v2013.12.05)\r\n",
				 "Accept: application/sdp\r\n\r\n">>,
	ok = gen_tcp:send(Socket, Discribe),
	
	{ok,DisReps} = gen_tcp:recv(Socket, 0),
	?INFO("~p -- dis reps:~p",[?MODULE,DisReps]),
	%% third, setup.
	Setup = <<"SETUP rtsp://10.3.0.86:50002/files/9min.avi/trackID=0 RTSP/1.0\r\n",
			  "CSeq: 4\r\nUser-Agent: LibVLC/2.1.2 (LIVE555 Streaming Media v2013.12.05)\r\n",
			  "Transport: RTP/AVP;unicast;client_port=14028-14029\r\n\r\n">>,
	ok = gen_tcp:send(Socket, Setup),
	
	{ok,SetupReps} = gen_tcp:recv(Socket, 0),
	?INFO("~p -- setup reps:~p~n",[?MODULE,SetupReps]),
	
	ok.





