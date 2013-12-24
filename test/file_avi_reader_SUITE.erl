%% Author: sunshine
%% Created: 2013-12-12
%% Description: TODO: Add description to file_avi_reader_SUITE
-module(file_avi_reader_SUITE).


%%
%% Include files
%%
-include("rtp_session.hrl").
-include("rtsp.hrl").
-include("media_info.hrl").
-include("log.hrl").
%%
%% Exported Functions
%%
-export([]).

-compile(export_all).
-define(FILE_READ_OPEN_OPTIONS,[read,raw,binary,read_ahead]).
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
	
%% 	test_get_file_medio_info,
	test_common
	 ].

test_get_file_medio_info(_) ->
	FilePath = "../../test/9min.avi",
	{ok,FD} = prim_file:open(FilePath,?FILE_READ_OPEN_OPTIONS),
	{ok,#media_info{} = MediaInfo,_,_,_} = file_avi_reader:decode_avi_file(FD),
	?INFO("~p~n",[MediaInfo]),
	ok.


test_common(_) ->
	FilePath = "../../test/video.avi",
	{ok,Pid} = file_avi_reader:start_link(),
	ok = file_avi_reader:init_file(Pid, FilePath),
	{ok,#media_info{video=Videos} = MediaInfo} = file_avi_reader:get_sdp(Pid),
	?INFO("sdp:~p",[MediaInfo]),
	ok = loop_fmtp(Videos),
	ReadId = 1,
	ok = file_avi_reader:position(Pid, 0, 0),
	file_avi_reader:read(Pid, self(), ReadId),
	timer:sleep(2000),
	
%% 	{error,not_found} = file_avi_reader:position(Pid, 10, 100),
%% 	ok = file_avi_reader:position(Pid, 4000, 0),
	ok = file_avi_reader:stop(Pid),
	ok.

receive_msg() ->
	receive
		[error] ->
			?INFO("~p -- receive error"),
			error;
		Frames ->
%% 			?INFO("~p -- receive frames length:~p",[?MODULE,length(Frames)]),
			ok
 		after 3*1000 ->
			exit(failed)
	end.
	
loop_fmtp([]) -> ok;
loop_fmtp([V|T]) ->
	Sdp = sdp:encode(V),
	?TRACK("video:~p~nsdp:~p~n",[V,Sdp]),
	loop_fmtp(T).
