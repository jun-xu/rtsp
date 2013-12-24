%% Author: sunshine
%% Created: 2013-12-23
%% Description: TODO: Add description to rtsp_source_reader
-module(rtsp_source_reader).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).
-export([start_link/0,get_sdp/1,position/3,pause/1,stop/1]).

%%
%% API Functions
%%
behaviour_info(callbacks) ->
    [{get_sdp,1},{start_link,0},
	 {stop,1},{pause,1},
	 {position,3}
	];
behaviour_info(_) ->
    undefined.


start_link() ->
	gen_server:start_link(?MODULE, [], []).

get_sdp(Pid) ->
	gen_server:call(Pid, get_sdp,infinity).

position(Pid,StartPos,Track) ->
	gen_server:call(Pid, {setup,StartPos,Track},infinity).

pause(Pid) ->
	gen_server:call(Pid, pause,infinity).

stop(Pid) ->
	gen_server:call(Pid, stop,infinity).

%%
%% Local Functions
%%

