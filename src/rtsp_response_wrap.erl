%% Author: Administrator
%% Description: TODO: Add description to rtsp_response_wrap
-module(rtsp_response_wrap).

%%
%% Include files
%%
-include("rtsp.hrl").
-include("error_code.hrl").
-include("log.hrl").
%%
%% Exported Functions
%%
-export([handle_reply/2,get_rtspserver_ip/1]).

%%
%% API Functions
%%

handle_reply(options, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_OK,
	  			"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
	  			?RTSP_OPTIONS_SUPPORT_FUNCTION,
	  			?FORMAT_LF_CR>>;

handle_reply(describe, {Headers, SDP_Part, URL}) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),	
	{ok, IP} = get_rtspserver_ip(URL),
	SDP = <<"v=0", ?FORMAT_LF_CR,
			"o=- 1328264151122832 1 IN IP4 ", IP/binary, ?FORMAT_LF_CR,
			SDP_Part/binary>>,
	Len = byte_size(SDP),
	_Respon = <<?RTSP_OK,
			   "CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
               ?DESCRIBE_CONTENT_TYPE,
               "Content-Length: ", (list_to_binary(integer_to_list(Len)))/binary, ?FORMAT_LF_CR,
			   ?FORMAT_LF_CR,
			   SDP/binary>>;

handle_reply(setup, {Headers, Source_IP, TranBin,Rtp_Port, Rtcp_Port, Session, IP}) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	{'Transport', Transport} = lists:keyfind('Transport', 1, Headers),
	{'client_port', {Client_port_rtsp, Client_port_rtp}} = lists:keyfind('client_port', 1, Transport),
	ClientPorts = lists:concat([Client_port_rtsp,"-",Client_port_rtp]),
	ServerPorts = lists:concat([Rtp_Port,"-",Rtcp_Port]),
	CseqStr = binary_to_list(Cseq),
	Respons = [?RTSP_OK,io_lib:format("CSeq: ~s",[CseqStr]), ?FORMAT_LF_CR,
			   "Date: Tue, Feb 07 2012 11:15:32 GMT",?FORMAT_LF_CR,
			   io_lib:format("Transport:~s;destination=~s;source=~s;client_port=~s;server_port=~s",[TranBin,IP,Source_IP,ClientPorts,ServerPorts]),?FORMAT_LF_CR,
			   io_lib:format("Session:~s",[Session]),?FORMAT_LF_CR,
			   ?FORMAT_LF_CR
			  ],
	iolist_to_binary(Respons);
handle_reply(setup, {Headers,TranBin,{RtpInterleaved,RtcpInterleaved},Session}) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_OK,
			    "CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			    "Date: Tue, Feb 07 2012 11:15:32 GMT",?FORMAT_LF_CR,
			    "Transport:",TranBin/binary,";",
			    "interleaved=",(list_to_binary(integer_to_list(RtpInterleaved)))/binary,"-",
							(list_to_binary(integer_to_list(RtcpInterleaved)))/binary,?FORMAT_LF_CR,
			    "Session:",Session,?FORMAT_LF_CR,
			    ?FORMAT_LF_CR>>;

handle_reply(play, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	StartRange = parse_start_range(Headers),
	{'Session', Session} = lists:keyfind('Session',1,Headers),
	_Respon = <<?RTSP_OK,
			   "CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			   "User-Agent: LibVLC/1.1.4 (LIVE555 Streaming Media v2010.08.22)\r\n",
               "Session: ",Session/binary,?FORMAT_LF_CR,
               "Range: npt=",(list_to_binary(StartRange))/binary,"-",?FORMAT_LF_CR,
			   ?FORMAT_LF_CR>>;

handle_reply(normal_reply, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	{'Session', Session} = lists:keyfind('Session',1,Headers),
	_Respon = <<?RTSP_OK,
			   	"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			   	"Session: ",Session/binary,?FORMAT_LF_CR,
			   	?FORMAT_LF_CR>>;

handle_reply(not_support_method, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_NOT_SUPPORT_FUNCTION,
			  "CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			  ?RTSP_OPTIONS_SUPPORT_FUNCTION,
		      ?FORMAT_LF_CR>>;
handle_reply(invalid_parameter, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_INVALID_PARAMETER,
			  "CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			  "Content-type: text/parameters",?FORMAT_LF_CR,
			  "Content-length: 0",?FORMAT_LF_CR,
		      ?FORMAT_LF_CR>>;



handle_reply(server_internal_error, Headers)->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_SERVER_INTERNAL_ERROR,
				"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
				?FORMAT_LF_CR>>;
handle_reply(session_not_find, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_SESSION_NOT_FOUND,
			  	"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			  	?FORMAT_LF_CR >>;
handle_reply(cannot_find, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_NOT_FIND,
			  	"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			  	?FORMAT_LF_CR >>;

handle_reply(bad_request, Headers)->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_BAD_REQUEST,
				"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
				?FORMAT_LF_CR>>;
handle_reply(setup_failed, Headers)->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_SETUP_FAILED,
				"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
				?FORMAT_LF_CR>>;

handle_reply(bad_play_range, Headers)->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_BAD_PLAY_RANGE,
				"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
				?FORMAT_LF_CR>>;

handle_reply(unsupported_transport, Headers) ->
	{'Cseq', Cseq} = lists:keyfind('Cseq', 1, Headers),
	_Respon = <<?RTSP_UNSUPPORTED_TRANSPORT,
			  	"CSeq: ", Cseq/binary, ?FORMAT_LF_CR,
			  	?FORMAT_LF_CR >>.

	
%% Local Functions
%%

parse_start_range(Headers) ->
	case proplists:get_value('Range', Headers) of
    	<<"npt=", Range/binary>> ->
      	[StartS |_] = string:tokens(binary_to_list(Range), "-"),
      	StartS;
    	_ ->
      	"0.000"
  	end.


get_rtspserver_ip(URL) ->
	case re:run(URL,"rtsp://([^/]+)+(:[^$]+)",[{capture,[1,2],binary}]) of
		{match,[IP,_Rest]} ->
			{ok,IP};
	nomatch ->
		case re:run(URL, "rtsp://([^/]+)+([^$]+)",[{capture,[1,2],binary}]) of
		{match,[IP,_Rest]} ->
			{ok,IP};
		_ ->
			error
		end
	end.

