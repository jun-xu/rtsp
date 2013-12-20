
-define(RTCP_SR, 200).
-define(RTCP_SD, 202).
-define(RTCP_BYE, 203).

-define(YEARS_70, 2208988800).

-define(RTP_OVER_TCP_MAGIC,16#24).



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

