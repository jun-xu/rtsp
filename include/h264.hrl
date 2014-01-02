
-define(NAL_SINGLE, 1).
-define(NAL_SLICE_A, 2).
-define(NAL_SLICE_B, 3).
-define(NAL_SLICE_C, 4).
-define(NAL_IDR, 5).
-define(NAL_SEI, 6).
-define(NAL_SPS, 7).
-define(NAL_PPS, 8).
-define(NAL_DELIM, 9).
-define(NAL_END_SEQ, 10).
-define(NAL_END_STREAM, 11).
-define(NAL_FILLER, 12).
-define(NAL_SPS_EXT, 13).
-define(NAL_STAP_A, 24).
-define(NAL_STAP_B, 25).
-define(NAL_MTAP16, 26).
-define(NAL_MTAP24, 27).
-define(NAL_FUA, 28).
-define(NAL_FUB, 29).

-define(H264_PKT_NONE, 0).
-define(H264_PKT_NONINT, 1).
-define(H264_PKT_INT, 2).


-define(DEFAULT_PAYLOAD_TYPE,96).

-record(h264, {
  profile,
  profile_compat = 0,
  flag,
  level,
  length_size = 32,
  sps,
  pps,
  buffer = undefined
}).