%% Author: sunshine
%% Created: 2013-12-23
%% Description: TODO: Add description to h264
-module(h264).

%%
%% Include files
%%
-include("h264.hrl").
-include("log.hrl").
%%
%% Exported Functions
%%
-export([to_fmtp/1]).

%%
%% API Functions
%%
to_fmtp(Config) ->
	SPS = proplists:get_value(sps, Config),
	PPS = proplists:get_value(pps, Config),
	case {SPS,PPS} of
		{undefined,_} -> undefined;
		{_,undefined} -> undefined;
		_ ->
			
			H264 = decode_sps_nal(SPS, #h264{}),
    		PLI =case decode_pps_nal(PPS, H264) of
      				#h264{profile = Profile,level = Level} when (is_integer(Profile) and is_integer(Level)) ->
        				io_lib:format("profile-level-id=~2.16.0B~2.16.0B~2.16.0B;", [Profile, 16#E0, Level]);
      				_ -> []
    			end,
  			PktMode = ?H264_PKT_NONINT,
 			[
   			"packetization-mode=", integer_to_list(PktMode),";",
  			PLI,
   			"sprop-parameter-sets=",
   			base64:encode(SPS), $,, base64:encode(PPS)
  			]
			
	end.
%%
%% Local Functions
%%
decode_sps_nal(undefined,H264) -> H264;
decode_sps_nal(<<0:1, _NalRefIdc:2, ?NAL_SPS:5, Profile, _:8, Level, _/binary>> = SPS, #h264{} = H264) ->
  H264#h264{profile = Profile, level = Level, sps = SPS}.

decode_pps_nal(undefined,H264) -> H264;
decode_pps_nal(<<0:1, _NalRefIdc:2, ?NAL_PPS:5, Bin/binary>> = PPS, #h264{} = H264) ->
  {_PPSId, Rest1} = exp_golomb_read(Bin),
  {_SPSId, _Rest} = exp_golomb_read(Rest1),
  H264#h264{pps = PPS}.

exp_golomb_read(Bin) ->
  exp_golomb_read(Bin, 0).

exp_golomb_read(<<0:1, Rest/bitstring>>, LeadingZeros) ->
  exp_golomb_read(Rest, LeadingZeros + 1);

exp_golomb_read(<<1:1, Data/bitstring>>, LeadingZeros) ->
  <<ReadBits:LeadingZeros, Rest/bitstring>> = Data,
  CodeNum = (1 bsl LeadingZeros) -1 + ReadBits,
  {CodeNum, Rest}.




