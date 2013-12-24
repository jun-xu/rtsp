
%% -type(stream_decoder_config() :: any()).
%% -type(stream_video_params() :: {non_neg_integer()}).

-record(video_params, {
  width  = 0 ::non_neg_integer(),
  height = 0 ::non_neg_integer(),
  fps = 0 ::non_neg_integer()
}).

-type(video_params() :: #video_params{}).

-record(audio_params, {
  channels    = 0 ::non_neg_integer(),
  sample_rate = 0 ::non_neg_integer()
}).

-define(FRAME_CONTENT_AUDIO,audio).
-define(FRAME_CONTENT_VIDEO,video).
-define(FRAME_CONTENT_METADATA,metadata).

-type(audio_params() :: #audio_params{}).
-type(frame_content() ::?FRAME_CONTENT_AUDIO|?FRAME_CONTENT_VIDEO|?FRAME_CONTENT_METADATA).
-type(frame_video_codec() ::h264|sorenson|vp6|vp6f|mpeg4).
-type(frame_audio_codec() ::aac|mp3|pcma|pcmu|pcm|pcm_le|g726_16|speex|nellymoser|nellymoser8).
-type(frame_codec()   ::frame_video_codec()|frame_audio_codec()|atom()).

-record(stream_info, {
  content        = undefined ::frame_content(),
  stream_id      = 0         ::non_neg_integer(),
  codec 	     = h264 ::frame_codec()|undefined,
  mic_sec_per_frame = 40,
%%   config         = undefined ::stream_decoder_config(),
  bitrate        = undefined ::non_neg_integer(),
  language       = 1 ::string()|undefined,
  params         = undefined ::audio_params()|video_params(),
  timescale      = 90000::non_neg_integer(), % h.264 90KHZ
  options        = []        ::any()
}).

-type(stream_info() :: #stream_info{}).

-define(FLOW_TYPE_FILE,file).
-define(FLOW_TYPE_STREAM,stream).

-type(flow_type() :: ?FLOW_TYPE_FILE|?FLOW_TYPE_STREAM).

-record(media_info, {
  flow_type  = undefined ::flow_type(),
  audio      = [] :: [stream_info()],
  video      = [] :: [stream_info()],
  metadata   = undefined :: stream_info(),
  duration   = undefined :: non_neg_integer()|undefined,
  options    = [] :: [any()]
}).
