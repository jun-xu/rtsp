%% Author: Administrator
%% Created: 2012-5-25
%% Description: TODO: Add description to adts_rtp_wrap
-module(adts_rtp_wrap).

%%
%% Include files
%%
-include("rtsp.hrl").
-include("rtp_session.hrl").
-include("log.hrl").

%%
%% Exported Functions
%%
-export([wrap/2]).

%%
%% API Functions
%%
-spec wrap(Frame::#frame{}, RtpState::#rtp_state{}) -> 
		  {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(Frame=#frame{timestamp=Timestamp, data=Data}, RtpState) ->
	Syncword = 16#fff,
	{SamplingFrequencyIndex, Body} =
		case Data of
			<<Syncword:12, 
			   _ID:1, 
			   _Layer:2, 
			   0:1,
			   _Profile:2, 
			   _SamplingFrequencyIndex:4,
			   _PrivateBit:1,
			   _ChannelConfiguration:3,
			   _OriginalOrCopy:1,
			   _Home:1,
			   _CopyrightIdentificationBit:1,
			   _CopyrightIdentificationStart:1,
			   _FrameLength:13,
			   _AdtsBufferFullness:11,
			   _NumberOfRawDataBlocksInFrame:2,
			  _Protection:16,
			  _Body/binary>> ->
				{_SamplingFrequencyIndex, _Body};		
			<<Syncword:12, 
			   _ID:1, 
			   _Layer:2, 
			   _ProtectionAbsent:1,
			   _Profile:2, 
			   _SamplingFrequencyIndex:4,
			   _PrivateBit:1,
			   _ChannelConfiguration:3,
			   _OriginalOrCopy:1,
			   _Home:1,
			   _CopyrightIdentificationBit:1,
			   _CopyrightIdentificationStart:1,
			   _FrameLength:13,
			   _AdtsBufferFullness:11,
			   _NumberOfRawDataBlocksInFrame:2,
			  _Body/binary>> ->
				{_SamplingFrequencyIndex, _Body}
		end,
	SamplingFrequency = get_sampling_frequency(SamplingFrequencyIndex),
	mpeg4_generic_rtp_wrap:wrap(Frame#frame{timestamp=Timestamp * SamplingFrequency div 1000, data=Body}, 
								RtpState#rtp_state{samplingfrequency = SamplingFrequency}).

%%
%% Local Functions
%%
%% Ques: Index > 12??
-spec get_sampling_frequency(Index :: [0..15]) -> SamplingFrequency :: integer()| {error,undefined}.
get_sampling_frequency(Index) ->
	case proplists:get_value(Index,?SAMPLING_FREQUENCY_MAPPING) of
		undefined ->
			{error,undefined};
		Value ->
			Value
	end.


