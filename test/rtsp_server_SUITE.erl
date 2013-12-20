
-module(rtsp_server_SUITE).
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
	ok = application:start(rtsp_server),
	
	
	ok.