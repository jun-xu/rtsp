
-define(DEFAULT_SCAL,1.0).
-define(BINARY_TO_FLOAT(Binary), list_to_float(binary_to_list(Binary))).

-define(RTP_SESSION_STATE_PLAYING,1).
-define(RTP_SESSION_STATE_PAUSING,2).

%% -------------- frame ----------------------------
-define(DEFAULT_PACKET_SIZE,16*1024).
-record(frame, {timestamp = 0 :: integer(),
				track = 1 :: integer(),
				type = h264 :: atom(),
				timescale = 90000,		%% h264: 90000HZ
				data :: binary(),
				duration
			   }).

-define(DEFAULT_FRAME_TYPE_MAPPING,default).
-define(FRAME_TYPE_MAPPING,[{0,h264},{1,aac}]).

-record(setup, {url :: string(),
				seq = 0 :: 0..16#ffff,
				ssrc = 0 :: 0..16#ffffffff,
				destination :: string(),
				client_port :: {integer(),integer()},
				socket :: port(),
				check_rr = false :: boolean(),
				transport = udp,
				interleaved = undefined,	%% for tcp only,
			    client_pid = undefined  %% for stream client only   
			   }).

-record(play, {scale = ?DEFAULT_SCAL :: float(),
			   range :: {StartTime::integer(), StopTime::integer()} | undefined %% undefined:recover pause
			   }).

-record(pause, {range :: integer()|undefined}).

%% -record(channel, {rtp_send_fun :: fun((#frame{},#rtp_state{},#rtcp_state{}) -> {ok, #rtp_state{}, #rtcp_state{}}),
%%                   rtp_state :: #rtp_state{},
%%                   rtcp_send_fun :: fun((Packet :: binary()) -> ok),
%% 				  rtcp_state ::#rtcp_state{}
%%                  }).

-record(rtp_session_state_ex, {session_id,
							file_begin_time = {{2012,12,12}, {12,12,12}},
							source_mod :: atom(),
							source_pid :: pid(),
%% 							sender_pid :: pid(),
							is_reading =false :: boolean(),
							rtp_state = ?RTP_SESSION_STATE_PAUSING,	
							read_id = 0 :: integer(),
							send_timer = undefined :: reference(),
%% 							tracks = []	::	[{Track::integer(),Type::string()}],
							channels= [] :: [{Track::integer(),{TransportMod::atom()  %% tcp: rtsp_socket  udp: rtp_udp_sender
														 ,Sender::pid()}}],     %% track 1:video    2: audio
							plays = [] :: [#play{}],
							current_play = undefined :: #play{},
							frames = [] :: [#frame{}],
							cache_frame_size = 30 :: integer(),
							
							rr_timer = 0 :: integer(),
							pre_frame = undefined :: undefined | {PreSendTime::integer(),PrePos::integer()},
							   
							sdp = undefined,
							media_info = undefined
						   }).

-record(rtp_session_streams,{session_id,
							 source_pid :: pid(),
							 source_ref,
							 rtp_state = ?RTP_SESSION_STATE_PAUSING,
							 channels= [] :: [{Track::integer(),{TransportMod::atom(),Sender::pid()}}],
							 sdp = undefined,
							 media_info = undefined     
							 }).

