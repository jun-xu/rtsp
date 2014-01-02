%% Author: sunshine
%% Created: 2013-12-27
%% Description: TODO: Add description to aac
-module(aac).
-include("log.hrl").
-include("aac.hrl").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([decode_config/1]).

%%
%% API Functions
%%

decode_config(AAC) ->
  extract_object_type(AAC, #aac_config{}).
  
extract_object_type(<<2#11111:5, ObjectType:6, AAC/bitstring>>, #aac_config{} = Config) ->
  extract_sample_rate(AAC, Config#aac_config{type = decode_object_type(ObjectType)});

extract_object_type(<<ObjectType:5, AAC/bitstring>>, #aac_config{} = Config) ->
  extract_sample_rate(AAC, Config#aac_config{type = decode_object_type(ObjectType)}).
  
extract_sample_rate(<<2#1111:4, SampleRate:24, AAC/bitstring>>, Config) ->
  extract_channels(AAC, Config#aac_config{sample_rate = SampleRate});

extract_sample_rate(<<SampleRate:4, AAC/bitstring>>, Config) ->
  extract_channels(AAC, Config#aac_config{sample_rate = decode_sample_rate(SampleRate)}).

extract_channels(<<Channels:4, FrameLength:1, _DependsCore:1, _Extension:1, _/binary>>, Config) ->
  Config#aac_config{channels = decode_channels(Channels), channel_count = Channels, samples_per_frame = decode_samples_per_frame(FrameLength)}.



%%
%% Local Functions
%%
decode_samples_per_frame(0) -> 1024;
decode_samples_per_frame(1) -> 960.

decode_channels(0) -> specific;
decode_channels(1) -> fc;
decode_channels(2) -> flr;
decode_channels(3) -> flcr;
decode_channels(4) -> flcr_bc;
decode_channels(5) -> flcr_blr;
decode_channels(6) -> flcr_blr_lfe;
decode_channels(7) -> flcr_slr_blr_lfe.

decode_object_type(0) -> null;
decode_object_type(1) -> aac_main;
decode_object_type(2) -> aac_lc;
decode_object_type(3) -> aac_ssr;
decode_object_type(4) -> aac_ltp;
decode_object_type(5) -> aac_sbr.

decode_sample_rate(0) -> 96000;
decode_sample_rate(1) -> 88200;
decode_sample_rate(2) -> 64000;
decode_sample_rate(3) -> 48000;
decode_sample_rate(4) -> 44100;
decode_sample_rate(5) -> 32000;
decode_sample_rate(6) -> 24000;
decode_sample_rate(7) -> 22050;
decode_sample_rate(8) -> 16000;
decode_sample_rate(9) -> 12000;
decode_sample_rate(10) -> 11025;
decode_sample_rate(11) -> 8000;
decode_sample_rate(12) -> 7350.
