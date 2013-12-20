%% Author: dhy
%% Created: 2012-2-15
%% Description: TODO: Add description to h264_rtp_wrap
-module(h264_rtp_wrap).

%%
%% Include files
%%
-include("rtsp.hrl").
-include("rtp_session.hrl").
-include("log.hrl").
-include("h264.hrl").


%%
%% Exported Functions
%%
-export([wrap/2]).

%%
%% API Functions
%%
-spec wrap(Frame::#frame{}, RtpState::#rtp_state{}) -> 
		  {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(#frame{timestamp=Timestamp, data=Data}, RtpState) ->
%% 	?DEBUG("~p --  send frame:~p",[?MODULE,{Track,Type,Timestamp}]),
	[<<>> | Nals] = binary:split(Data, <<1:32>>, [global]),
	RtpTimestamp = Timestamp * 90000 div 1000,
	wrap(RtpTimestamp, Nals, RtpState, []).

%%
%% Local Functions
%%

-spec wrap(Timestamp::integer(), Nals::list(binary()), RtpState::#rtp_state{}, Ret::list(binary())) -> 
		  {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(_Timestamp, [], RtpState, Ret) -> {ok, lists:reverse(Ret), RtpState};
wrap(Timestamp, [Nal | T], RtpState, Ret) ->
	wrap(Timestamp, Nal, T, RtpState, Ret).

-spec wrap(Timestamp::integer(), Nal::binary(), OtherNals::list(binary()), RtpState::#rtp_state{}, Ret::list(binary())) -> 
		  {ok, Rtps::list(), NewRtpState::#rtp_state{}}.


wrap(Timestamp, Nal, OtherNals, RtpState, Ret) when size(Nal) =< 1436 ->
	Marker = case OtherNals of
			[] -> 1;
			_-> 0
		end,
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,Nal/binary>>,
	wrap(Timestamp, OtherNals, RtpState#rtp_state{seq = (Seq+1) rem 16#10000}, [Rtp | Ret]);
wrap(Timestamp, <<0:1, NRI:2, Type:5, Data:1434/binary, Rest/binary>>, OtherNals, RtpState, Ret) ->
	S = 1,
	E = 0,
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	FU = <<0:1, NRI:2, ?NAL_FUA:5, S:1, E:1, 0:1, Type:5, Data/binary>>,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, 0:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,FU/binary>>,
	wrap(Timestamp, NRI, Type, Rest, OtherNals, RtpState#rtp_state{seq = (Seq+1) rem 16#10000}, [Rtp | Ret]).

-spec wrap(Timestamp::integer(), 
		   NRI::integer(), 
		   Type::integer(), 
		   Rest::binary(), 
		   OtherNals::list(binary()), 
		   RtpState::#rtp_state{}, 
		   Ret::list(binary())) -> 
		  {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(Timestamp, NRI, Type, Data, OtherNals, RtpState, Ret) when size(Data) =< 1434 ->
	S = 0,
	E = 1,
	Marker = case OtherNals of
			[] -> 1;
			_->0
		end,
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	FU = <<0:1, NRI:2, ?NAL_FUA:5, S:1, E:1, 0:1, Type:5, Data/binary>>,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,FU/binary>>,
	wrap(Timestamp, OtherNals, RtpState#rtp_state{seq = (Seq+1) rem 16#10000}, [Rtp | Ret]);
wrap(Timestamp, NRI, Type, <<Data:1434/binary, Rest/binary>>, OtherNals, RtpState, Ret) ->
	S = 0,
	E = 0,
	Marker = 0,
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	FU = <<0:1, NRI:2, ?NAL_FUA:5, S:1, E:1, 0:1, Type:5, Data/binary>>,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,FU/binary>>,
	wrap(Timestamp, NRI, Type, Rest, OtherNals, RtpState#rtp_state{seq = (Seq+1) rem 16#10000}, [Rtp | Ret]).
