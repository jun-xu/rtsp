
-define(APP_NAME,rtsp_server).

-define(RTP_AVP,"RTP/AVP").
-define(DEFAULT_LISTEN_PORT,56000).

-define(SND_BUF_SIZE, (512 * 1024)).

-define(INTEGER_TO_BINARY(Integer), list_to_binary(integer_to_list(Integer))).


-define(RTSP_OPTIONS_SUPPORT_FUNCTION, "Public: OPTIONS, DESCRIBE, SETUP, PLAY, PAUSE, TEARDOWN\r\n").
-define(DESCRIBE_CONTENT_TYPE,"Content-Type: application/sdp\r\n").

-define(RTSP_PART_SDP,  "v=0\r\n",
						"o=- 1328195404452049 1 IN IP4 192.168.203.180\r\n",
						"s=H.264 Video, streamed by the Bingo RTSP Media Server\r\n",
						"i=60.264\r\n",
						"t=0 0\r\n",
						"a=tool:Bingo RTSP Streaming Media v2012.2.20\r\n",
						"a=type:broadcast\r\n",
						"a=control:*\r\n",
						"a=range:npt=0-331.018\r\n",
						"a=x-qt-text-nam:H.264 Video, streamed by the Bingo RTSP Media Server\r\n",
						"a=x-qt-text-inf:60.264\r\n").

-define(FORMAT_LF_CR, "\r\n").


-define(DEFAULT_RTSP_SEQ_START,1234).


-define(DEFAULT_SAMPLING_FREQUENCY,90000).
-define(SAMPLING_FREQUENCY_MAPPING,[{0,96000},{1,88200},{2,64000},{3,48000},{4,44100},{5,32000},{6,24000},
								    {7,22050},{8,16000},{9,12000},{10,11025},{11,8000},{12,7350}]).
-record(rtp_state, {seq=0 :: integer(),
					ssrc=0 :: integer(),
					dtc,
					samplingfrequency = ?DEFAULT_SAMPLING_FREQUENCY :: integer()}).
-record(rtp_udp_sender,{client_rtp_port :: integer(),
					server_rtp_port :: integer(),
					server_rtcp_port :: integer(),
					rtp_socket,
					rtcp_socket,
					rtcp_pid :: pid(),
					destination,
					rtp_state = #rtp_state{},
					setup=false
  		}).

-record(rtp_stream_udp_sender,{senders = [] :: [{Pid::pid(),#rtp_udp_sender{}}]
  		}).


-define(RTP_VERSION,2).
-define(RTP_NO_PADDING,0).
-define(RTP_NO_EXTANSION,0).
-define(RTP_NO_CSRC,0).

-define(RTCP_SR, 200).
-define(RTCP_SD, 202).
-define(RTCP_BYE, 203).

-define(YEARS_70, 2208988800).

-define(RTP_OVER_TCP_MAGIC,16#24).

-define(RTSP_SERVER_FILES_MOD,rtp_session_ex).
-define(RTSP_SERVER_STREAMS_MOD,rtp_session_stream).
-define(DEFAULT_RTSP_SERVER_MOD,?RTSP_SERVER_FILES_MOD).

-define(RTSP_SERVER_ROUTE_MOD_MAPPING,[{files,?DEFAULT_RTSP_SERVER_MOD},{"files",?DEFAULT_RTSP_SERVER_MOD},
									   {streams,?RTSP_SERVER_STREAMS_MOD},{"streams",?RTSP_SERVER_STREAMS_MOD}]).

-define(AVI_READER_MOD,file_avi_reader).
-define(MP4_READER_MOD,file_mpg4_reader).
-define(RTSP_FILE_MOD_MAPPING,[{".avi",?AVI_READER_MOD},{".mp4",?MP4_READER_MOD}]).




-record(url, {protocol :: string(),
			  host :: string(),
			  port :: integer(),
			  path = [] :: list(string()),
			  params = [] :: list(tuple())}).

-record(rtcp_state0, {sampling_frequency = ?DEFAULT_SAMPLING_FREQUENCY ::integer(),
			   packet_count = 0 ::integer(),
			   octet_count = 0 ::integer()
			  }).

-record(rtcp_state, {rtcp_pid ::pid(),
			   track = 1 ::integer(),
			   sampling_frequency = ?DEFAULT_SAMPLING_FREQUENCY ::integer(),
			   packet_count = 0 ::integer(),
			   octet_count = 0 ::integer()
			  }).

-record(rtcp, {rtp_session_pid ::pid(),
			   destination ::string(),
			   rtcp_port ::integer(),
			   recv_rtcp_number = 0 ::integer(),
			   is_send = false ::boolean(),
			   rtcp_socket :: port(),
			   identifier = 0 ::integer(),
			   rtcp_state =#rtcp_state0{} ::#rtcp_state0{}
			  }).
