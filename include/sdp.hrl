% -record(payload, {
%           num,
%           codec,
%           clock_map,
%           ms      :: undefined | mono | stereo,
%           ptime   :: integer(),
%           config = []
%          }).
% 
-type(sdp_attr() :: atom() | {atom(), string() | binary()}).
% 
% -record(media_desc, {
%   type,
%   connect,
%   port,
%   payloads       = []    :: [#payload{}],
%   track_control,
%   pps,
%   sps,
%   config,
%   attrs          = []   :: [sdp_attr()]
% }).

-define(SDP_VERSION,0).
-define(SDP_CODEC,[
  				{h264, "H264"},
  				{h263, "H263"},
  				{aac, "mpeg4-generic"},
  				{pcma, "PCMA"},
  				{pcmu, "PCMU"},
  				{g726_16, "G726-16"},
 				{mpa, "MPA"},
  				{mp4a, "MP4A-LATM"},
  				{mp4v, "MP4V-ES"},
  				{mp3, "mpa-robust"},
  				{pcm, "L16"},
  				{speex, "speex"},
  				{g722, "G722"},
  				{ilbc, "iLBC"},
  				{g728, "G728"},
  				{dvi4, "DVI4"},
  				{telephone, "telephone-event"},
  				{mpegts, "MP2T"}]).

-record(sdp_o, {
  username = <<"-">>,
  sessionid,
  version,
  netaddrtype = inet4  :: inet4 | inet6,
  address :: string()
}).

-define(DEFAULT_SDP_NAME,"sun.xj rtsp v1").

-record(sdp_session, {
  version = ?SDP_VERSION,
  originator :: #sdp_o{},
  name = ?DEFAULT_SDP_NAME,
  connect,
  time     = {0, 0} :: {integer(), integer()},
  attrs    = []   :: [sdp_attr()]
}).


-define(TRACK_TAG,"trackID").

