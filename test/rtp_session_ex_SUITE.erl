%% Author: sunshine
%% Created: 2013-5-28
%% Description: TODO: Add description to rtp_session_ex_SUITE
-module(rtp_session_ex_SUITE).


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
%% 	test_jump
%% 	test_pause,
%% 	test_tcp
	 ].

test_common(_) ->
	URL = "http://127.0.0.1:8554/files/9min.avi",
	MockPid = spawn(fun() -> receive stop -> ok end end),
	root_dir_manager:start_link(),
	root_dir_manager:add_root_dir("/home/sunshine/git/rtsp/test"),
	{ok,Pid} = rtp_session_ex:start_link(),
	{ok,SDP,SessionId} = rtp_session_ex:get_sdp(Pid, URL),
	SetupURL = URL ++"/trackID=0",
	{error,bad_request} = rtp_session_ex:setup(Pid, #setup{seq=0,ssrc=2,destination="127.0.0.1",client_port={65010,65011},url=URL}),
	{ok,Ports}= rtp_session_ex:setup(Pid, #setup{seq=0,ssrc=2,destination="127.0.0.1",client_port={65010,65011},url=SetupURL}),
	{ok,Ports}= rtp_session_ex:setup(Pid, #setup{seq=0,ssrc=2,destination="127.0.0.1",client_port={65010,65011},url=SetupURL}),
%% 	ok = rtp_session_ex:play(Pid,#play{scale=2.0,range={1000,1500}}),
%% 	ok = rtp_session_ex:play(Pid,#play{scale=2.0,range={1000,undefined}}),
	ok = rtp_session_ex:play(Pid,#play{scale=1.0}),
%% 	ok = rtp_session_ex:play(Pid,#play{scale=undefined}),

	timer:sleep(3000),
	root_dir_manager:stop(),
	ok.




loop_gen_jump_data(0,B,_) -> B;
loop_gen_jump_data(Times,B,Pos) ->
%% 	<<Track:8, TypeIndex:8, Timestamp:48, Data:DataLen/binary, Rest/binary>>
	case Times rem 10 of
		0 ->
			CPos = Pos-10000,
			Data = <<(20*1024):32,0:8,20:8,CPos:48,0:(20*1024*8 - 64)>>,
			loop_gen_jump_data(Times-1,<<Data/binary,B/binary>>,CPos);
		_ ->
			CPos = Pos-40,
			Data = <<(20*1024):32,0:8,20:8,CPos:48,0:(20*1024*8 - 64)>>,
			loop_gen_jump_data(Times-1,<<Data/binary,B/binary>>,CPos)
	end.

loop_gen_data(0,B) -> B;
loop_gen_data(Times,B) ->
%% 	<<Track:8, TypeIndex:8, Timestamp:48, Data:DataLen/binary, Rest/binary>>
	Data = <<(20*1024):32,0:8,20:8,(Times*40):48,0:(20*1024*8 - 64)>>,
	loop_gen_data(Times-1,<<Data/binary,B/binary>>).








