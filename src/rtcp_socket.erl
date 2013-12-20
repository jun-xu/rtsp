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
-export([stop/1]).



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
	{noreply, State};

handle_info(Info, State) ->
	?DEBUG("~p -- handle info:~p~n",[?MODULE,Info]),
    {noreply, State}.


terminate(Reason,#rtcp{rtcp_socket = Socket}) ->
	?DEBUG("~p -- rtcp socket terminate by reason:~p~n",[?MODULE,Reason]),
	catch gen_udp:close(Socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
-spec send_bye_packet(State::#state{}) ->ok.
send_bye_packet(#rtcp{rtcp_socket = Rtcp_socket, destination = IP, rtcp_port = Port, identifier = Identifier}) ->
	?INFO("~p -- send bye to {~p,~p}.~n",[?MODULE,IP,Port]),
	Respond = wrap_bye(Identifier),
	ok = gen_udp:send(Rtcp_socket, IP, Port, Respond).


wrap_bye(SSRC) ->
	<<2:2, 0:1, 0:5, ?RTCP_SR:8, 6:16, SSRC:32,0:32,0:32,0:32,0:32,0:32,
	  2:2, 0:1, 1:5, ?RTCP_BYE:8, 1:16, SSRC:32>>.	





