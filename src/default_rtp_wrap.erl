%% Author: dhy
%% Created: 2012-5-30
%% Description: TODO: Add description to non_standard_rtp_wrap
-module(default_rtp_wrap).

%%
%% Include files
%%
-include("rtsp.hrl").
-include("rtp_session.hrl").
-include("log.hrl").

-define(MAX_RTP_PACKET_DATA_SIZE,1316).   %% 1316 = size(TS) * 7 = 188byte * 7

%%
%% Exported Functions
%%
-export([wrap/2]).

%%
%% API Functions
%%
-spec wrap(Frame::#frame{}, RtpState::#rtp_state{}) -> 
		  {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(#frame{timestamp=Timestamp, track=Track, data=Data}, RtpState) ->
	wrap(Timestamp, Track, Data, RtpState, []).

%%
%% Local Functions
%%
-spec wrap(Timestamp::integer(),
		   Track::integer(),
		   Data::binary(),
		   RtpState::#rtp_state{},
		   Ret::list(binary())) ->
		  {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(Timestamp, Track, Data, RtpState=#rtp_state{seq=Seq, ssrc=SSRC}, Ret) when size(Data) =< ?MAX_RTP_PACKET_DATA_SIZE ->	
	Marker = 1,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, (95+Track):7, Seq:16,
			Timestamp:32,   
			SSRC:32,
			Data/binary>>,	
	{ok, lists:reverse(Ret, [Rtp]), RtpState#rtp_state{seq = (Seq+1) rem 16#10000}};
wrap(Timestamp, Track, <<Data1:?MAX_RTP_PACKET_DATA_SIZE/binary, Data2/binary>>, RtpState=#rtp_state{seq=Seq, ssrc=SSRC}, Ret) ->
	Marker = 0,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, (95+Track):7, Seq:16,
			Timestamp:32,   
			SSRC:32,
			Data1/binary>>,	
	wrap(Timestamp, Track, Data2, RtpState#rtp_state{seq = (Seq+1) rem 16#10000}, [Rtp | Ret]).
