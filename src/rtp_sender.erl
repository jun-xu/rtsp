%% Author: sunshine
%% Created: 2013-5-30
%% Description: TODO: Add description to rtp_sender
-module(rtp_sender).

-include("log.hrl").
-include("rtp_session.hrl").
-include("rtsp.hrl").
-define(TRANSPORT_PROTO_MOD,[{"RTP/AVP",rtp_udp_sender},{"udp",rtp_udp_sender},{udp,rtp_udp_sender}]).

-define(DEFAULT_TOTLE_MEM,(1*1024*1024*1024)).
-define(DEFAULT_MEM_WEIGHT_UNIT,(512*1024*1024)).

%%
%% Exported Functions
%%


-export([behaviour_info/1]).
-export([get_transport_mod/1,start_link/0]).
-export([setup/2,stop/1,send/2,send_bye/1]).
%%
%% API Functions
%%
behaviour_info(callbacks) ->
    [{setup,2},{start_link,0},
	 {stop,1},{send,2},
	 {send_bye,1}
	];
behaviour_info(_) ->
    undefined.


%%
%% Local Functions
%%
get_transport_mod(ModType) ->
	proplists:get_value(ModType, ?TRANSPORT_PROTO_MOD, rtp_udp_sender).

start_link() ->
	{error,not_implement}.

-spec setup(Pid::pid(),Setup::#setup{}) -> ok | {ok, ServerPort::{RTPPort::integer(),RTCPPort::integer()}}.
											%% if transport is "RTP/AVP/TCP"  -> ok
											%% else "RTP/AVP/UDP"|"RTP/AVP"|"RTP/AVT"  -> {ok, ServerPort}
setup(Pid,Setup) ->
	{error,not_implement}.

send_bye(Pid) ->
	{error,not_implement}.

send(Pid,Frame) ->
	{error,not_implement}.

stop(Pid) ->
	{error,not_implement}.
