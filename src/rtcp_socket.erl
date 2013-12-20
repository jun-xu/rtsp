%%% -------------------------------------------------------------------
%%% Author  : an
%%% Description :
%%%
%%% Created : 2012-10-11
%%% -------------------------------------------------------------------
-module(rtcp_socket).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rtsp.hrl").
-include("log.hrl").

-define(RTCP_RR,201).
-define(RTCP_SDES,202).
-define(SEND_INTERVAL_TIME,3).
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,rtcp_start/4,rtcp_stop/1,rtcp_update/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%only for test
-export([split_rtcp_data/2,decode_rtcp_data/2, handle_rtcp_packet/2,stop/1]).



-record(state, {rtp_session_pid ::pid(),
			   destination ::string(),
			   rtcp_port ::integer(),
			   recv_rtcp_number = 0 ::integer(),
			   is_send = false ::boolean(),
			   rtcp_socket :: port(),
			   identifier = 0 ::integer()
			  }).

%% ====================================================================
%% External functions
%% ====================================================================
-spec rtcp_start(Port::integer(),Destination::string(),RtcpPort::integer(),RtcpSocket::port()) -> {ok,Pid::pid()}.
rtcp_start(Port,Destination,RtcpPort,RtcpSocket)->
	{ok, Pid} = rtcp_socket:start_link(),
    ok = gen_server:call(Pid, {open_rtcp_socket, Port,Destination,RtcpPort,RtcpSocket}),
	{ok, Pid}.

-spec rtcp_stop(Pid::pid()) -> ok.
rtcp_stop(Pid) ->
	gen_server:cast(Pid, send_bye).

-spec rtcp_update(RTCP::#rtcp_state0{},Pid::pid()) ->ok.
rtcp_update(RTCP,Pid) ->
	gen_server:cast(Pid, {rtcp_info, RTCP}).

start_link()->
	gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).
	

%% ====================================================================
%% Server functions
%% ====================================================================


init([]) ->
    {ok, #rtcp{}}.


handle_call({open_rtcp_socket, Port,Destination,RtcpPort,RtcpSocket}, {Pid, _Tag}, State) ->
%%     case gen_udp:open(Port, [{active, true}, binary]) of
%% 				{ok, Rtcp_socket} ->
%% 					gen_udp:controlling_process(Rtcp_socket, self()),
%% 					?DEBUG("~p -- rtcp client socket start on port:~p.~n",[?MODULE,Port]),
%% 					{reply, ok, State#rtcp{rtcp_socket = Rtcp_socket,destination=Destination,rtcp_port=RtcpPort,
%% 										   rtp_session_pid = Pid}};
%% 				{error, Reason} ->
%% 					?INFO("~p -- rtcp client socket start error by reason:~p.~n",[?MODULE,Reason]),
%% 					{reply,{error,Reason}, State}
%% 	end.
	?DEBUG("~p -- rtcp client socket start on port:~p ,server socket start on port:~p.~n",[?MODULE,RtcpPort,Port]),
	{reply, ok, State#rtcp{rtcp_socket = RtcpSocket,destination=Destination,rtcp_port=RtcpPort,
										   rtp_session_pid = Pid}}.

handle_cast({rtcp_info, RTCP}, State)->
	{noreply, State#rtcp{rtcp_state = RTCP}};


handle_cast(send_bye, State) ->
	send_bye_packet(State),
	{noreply,State};
handle_cast(stop,State) ->
	{stop, normal, State}.


handle_info({udp, Socket, IP, InPortNo, Packet},State) ->
%% 	?DEBUG("~p -- handle packet:~p~n",[?MODULE,Packet]),
	Parse_state = handle_rtcp_packet(Packet,State),
	Check_state = check_send_state(Parse_state),
	New_state = Check_state#rtcp{destination = IP,rtcp_port = InPortNo},
	After_send_state = send_rtcp_packet(Socket, New_state),	
	{noreply, After_send_state};

handle_info(Info, State) ->
	?DEBUG("~p -- handle info:~p~n",[?MODULE,Info]),
    {noreply, State}.


terminate(Reason, _State=#rtcp{rtcp_socket = Socket}) ->
	?DEBUG("~p -- rtcp socket terminate by reason:~p~n",[?MODULE,Reason]),
	catch gen_udp:close(Socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

-spec check_send_state(State::#state{}) ->State::#state{}.
check_send_state(State=#rtcp{recv_rtcp_number = Recv_number})->
	case Recv_number + 1 of
 		   ?SEND_INTERVAL_TIME ->
 				State#rtcp{recv_rtcp_number = 0 , is_send = true};
 			Number ->
				State#rtcp{recv_rtcp_number = Number,is_send = false}
	end.

-spec send_rtcp_packet(Socket::port(),State::#rtcp{}) -> State::#rtcp{}.
send_rtcp_packet(_Socket, State=#rtcp{is_send = Is_send}) ->
    case Is_send of
		true ->
%%    			send_sr_packet(Socket,State),	%%now open ,the picture will rocking
			State#rtcp{is_send = false};
		false ->
			State
	end.


-spec send_bye_packet(State::#state{}) ->ok.
send_bye_packet(_State=#rtcp{rtcp_socket = Rtcp_socket, destination = IP, rtcp_port = Port, identifier = Identifier}) ->
	?INFO("~p -- send bye to {~p,~p}.~n",[?MODULE,IP,Port]),
	Respond = rtcp_wrap:wrap_bye(Identifier),
	ok = gen_udp:send(Rtcp_socket, IP, Port, Respond).

-spec handle_rtcp_packet(Packet::binary(), State::#rtcp{}) -> State::#rtcp{}.
handle_rtcp_packet(Packet,State=#rtcp{identifier = Identifier}) ->
%% 	List = split_rtcp_data(Packet,[]),
%% 	case Identifier =:= 0 of
%% 		true ->
%% 			NewIdentifier = decode_rtcp_data(List,undefined);
%% 		false ->
%% 			decode_rtcp_data(List,undefined),   %%Ques: do nothing of state??
%% 			NewIdentifier = Identifier
%% 	end,
	State#rtcp{identifier = Identifier}.%%now nothing args is need,so do nothing


-spec split_rtcp_data(Data::binary(),List::list()) -> ok.
split_rtcp_data(<<>>, List) ->
	lists:reverse(List);
split_rtcp_data(Data, List) ->	
	<<Pre:8,Pt_sd:8,Length:16,Rest/binary>> = Data,
 	Real_size = Length * 4,%%real size is 4 times of this
	<<PartData:Real_size/binary, Rest_more/binary>> = Rest,
	Packet_one = <<Pre:8,Pt_sd:8,Length:16,PartData:Real_size/binary>>,
	List_new = [{Pt_sd,Packet_one}|List],
	split_rtcp_data(Rest_more,List_new).

-spec decode_rtcp_data(List::list(),Identifier::integer()) -> Identifier::integer().
decode_rtcp_data([],Identifier)->
	Identifier;
decode_rtcp_data([{?RTCP_RR,Data}|Rest],_) ->
	decode_rtcp_data(Rest,decode_rr(Data));
decode_rtcp_data([{?RTCP_SDES,Data}|Rest],Identifier) ->
	decode_sd(Data),
	decode_rtcp_data(Rest,Identifier);
decode_rtcp_data([_|Rest],Identifier) ->
	decode_rtcp_data(Rest,Identifier).

%% decode_rtcp_data([Head|Rest],Identifier)->
%% 	case Head of
%% 		{?RTCP_RR,Data} ->
%% 			decode_rtcp_data(Rest,decode_rr(Data));
%% 		{?RTCP_SDES, Data} ->
%% 			decode_sd(Data),
%% 			decode_rtcp_data(Rest,Identifier);
%% 		_ ->
%% 			decode_rtcp_data(Rest,Identifier)
%% 	end.	

-spec decode_rr(Data::binary) -> ok.
decode_rr(<<_V:2, _P:1, _Subtype:5, _Pt:8, 1:16, _SSRC:32>>) ->
	0;
decode_rr(Data) ->
	<<_V:2, _P:1, _Subtype:5, _Pt:8, _Length:16, _SSRC:32, Identifier:32
		   			 , _Faction_lost:8
		   			 , _Cumulative_number_of_packets_lost:24
		   			 , _Extended_Highest_Sequence_Number_received:32
		   			 , _Interarrival_jitter:32
		   			 , _LSR:32
		   			 , _DLSR:32, _Rest/binary>>  = Data,
	Identifier.


-spec decode_sd(Data::binary) -> ok.
decode_sd(Data) ->
	<<_V:2, _P:1, _Subtype:5, _Pt:8, _Length:16, Rest/binary>> = Data,
	decode_sd_chunk(Rest),
	ok.

-spec decode_sd_chunk(Data::binary) -> ok.	
decode_sd_chunk(<<>>) ->
	ok;
decode_sd_chunk(Data) ->
	<<_SSRC_sd:32, _Type_sd:8,Length_sd:8, Rest/binary>> = Data,
	<<_Text_sd:Length_sd/binary,_Rest1/binary>> = Rest,%%handle part of packet, the other part donot handle
	ok.





