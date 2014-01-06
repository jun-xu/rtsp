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
-spec wrap(Frame::#frame{}, RtpState::#rtp_state{}) -> {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(#frame{data=Data,duration=Duration},RtpState) ->
%% 	?TRACK("~p --  send frame:~p",[?MODULE,{Track,Type,Timestamp,Duration}]),
%% 	[<<>> | Nals] = binary:split(Data, <<1:32>>, [global]),
%% 	RtpTimestamp = Timestamp * 90000 div 1000,
	wrap(Duration, [Data], RtpState#rtp_state{dtc=rtsp_util:dtc()}, []).

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
wrap(Timestamp, Nal, OtherNals, RtpState, Ret) when size(Nal) =< 1388 ->
	Marker = case OtherNals of
			[] -> 1;
			_-> 0
		end,
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,Nal/binary>>,
	wrap(Timestamp, OtherNals, RtpState#rtp_state{seq = inc_seq(Seq)}, [Rtp | Ret]);
%% wrap(Timestamp, <<0:1, 00:2, Type:5,Rest/binary>>, OtherNals, RtpState, Ret) ->
%% 	wrap(Timestamp, <<0:1, 10:2, Type:5,Rest/binary>>, OtherNals, RtpState, Ret);
wrap(Timestamp, <<0:1, NRI:2, Type:5, Data:1387/binary, Rest/binary>>, OtherNals, RtpState, Ret) ->
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	FU = <<0:1, NRI:2, ?NAL_FUA:5, 1:1, 0:1, 0:1, Type:5, Data/binary>>,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, 0:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,FU/binary>>,
	wrap(Timestamp, NRI, Type, Rest, OtherNals, RtpState#rtp_state{seq = inc_seq(Seq)}, [Rtp | Ret]).

-spec wrap(Timestamp::integer(),NRI::integer(),Type::integer(),Rest::binary(),OtherNals::list(binary()),
		   RtpState::#rtp_state{},Ret::list(binary())) -> {ok, Rtps::list(), NewRtpState::#rtp_state{}}.
wrap(Timestamp, NRI, Type, Data, OtherNals, RtpState, Ret) when size(Data) =< 1387 ->
	Marker = case OtherNals of
			[] -> 1;
			_->0
		end,
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	FU = <<0:1, NRI:2, ?NAL_FUA:5, 0:1, 1:1, 0:1, Type:5, Data/binary>>,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,FU/binary>>,
	wrap(Timestamp, OtherNals, RtpState#rtp_state{seq = inc_seq(Seq)}, [Rtp | Ret]);
wrap(Timestamp, NRI, Type, <<Data:1387/binary, Rest/binary>>, OtherNals, RtpState, Ret) ->
	Marker = 0,
	#rtp_state{seq=Seq, ssrc=SSRC} = RtpState,
	FU = <<0:1, NRI:2, ?NAL_FUA:5, 0:1, 0:1, 0:1, Type:5, Data/binary>>,
	Rtp = <<?RTP_VERSION:2, ?RTP_NO_PADDING:1, ?RTP_NO_EXTANSION:1, ?RTP_NO_CSRC:4, Marker:1, ?DEFAULT_PAYLOAD_TYPE:7, 
			Seq:16,Timestamp:32,SSRC:32,FU/binary>>,
	wrap(Timestamp, NRI, Type, Rest, OtherNals, RtpState#rtp_state{seq = inc_seq(Seq)}, [Rtp | Ret]).

inc_seq(Seq) ->
  (Seq+1) band 16#FFFF.
