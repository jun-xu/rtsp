%% Author: sunshine
%% Created: 2014-1-6
%% Description: TODO: Add description to rtp_session
-module(rtp_session).

%%
%% Include files
%%
-include("rtp_session.hrl").
-include("sdp.hrl").
-include("media_info.hrl").
-include("rtsp.hrl").
-include("video_file.hrl").
-include("log.hrl").
%%
%% Exported Functions
%%
-export([behaviour_info/1]).
-export([start_link/0,setup/2,play/2,stop/1,pause/2,get_sdp/2]).
%%
%% API Functions
%%
behaviour_info(callbacks) ->
    [{start_link,0},{pause,2},
	 {stop,1},{send,2},
	 {get_sdp,1},{play,2}
	];
behaviour_info(_) ->
    undefined.


%%
%% API Functions
%%
start_link() ->
	{error,not_implement}.

-spec setup(Pid::pid(), Setup::#setup{}) -> {ok,ServerPort::{0..65536,0..65536}} | {error,Reason::atom()}.
setup(_Pid,_Setup) ->
	{error,not_implement}.

play(_Pid,_Play) ->
	{error,not_implement}.

pause(_Pid,_Pause) ->
	{error,not_implement}.

-spec get_sdp(Pid::pid(),URL::string()|binary()) -> {ok, SDP::string()} | error.
get_sdp(_Pid,_URL) ->
	{error,not_implement}.

stop(_Pid) ->
	{error,not_implement}.


%%
%% Local Functions
%%

