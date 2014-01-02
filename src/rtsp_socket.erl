%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-2-2
%%% -------------------------------------------------------------------
-module(rtsp_socket).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rtsp.hrl").
-include("rtp_session.hrl").
-include("log.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0,set_socket/2,start_link/0,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket :: port(),
				ip,
				port :: integer(),
				buf = <<>> :: binary(),
				url,
				rtp_pid       ::pid(),
				session = undefined   ::string()
				}).
%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

set_socket(Pid,Socket) ->
	gen_server:cast(Pid,{socket_ready,Socket}).

stop(Pid) ->
	gen_server:cast(Pid, stop).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};


handle_cast({socket_ready, Socket}, State) ->
	{ok, {IP, Port}} = inet:peername(Socket),
	inet:setopts(Socket, [binary,{active, once}]),
    {noreply, State#state{socket=Socket,ip=IP,port=Port}};
handle_cast(Msg,State) ->
	?INFO("~p -- no implement cast msg:~p",[?MODULE,Msg]),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State) ->
	?INFO("~p -- client closed.",[?MODULE]),
%% 	case State#state.rtp_pid of
%% 		undefined ->
%% 			{stop, normal, State};
%% 		_ ->
%% 			{stop,normal,State}
%%  	end;
	{stop,normal,State};
handle_info({tcp_error, _Socket, Resaon}, State) ->
	{stop,Resaon, State};
handle_info({tcp, Socket, Data}, State=#state{buf=Buf}) ->	
	inet:setopts(Socket, [binary,{active, once}]),
	handle_packet(State#state{buf = <<Buf/binary, Data/binary>>});
handle_info({'EXIT',_Pid,_Reason}, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, #state{rtp_pid=RTSPPid}) ->
	?INFO("~p -- terminate by reason:~p",[?MODULE,Reason]),
	case RTSPPid of
		undefined -> ok;
		RtspPid ->
			catch rtp_server:teardown(RtspPid)
	end,
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions

%% --------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec handle_packet(State::tuple()) -> 
%%		  {noreply, NewState::tuple()} | {stop, Reason::any(), NewState::tuple()}.
%%----------------------------------------------------------------------
handle_packet(State=#state{buf=Buf,socket=Socket}) ->
%% 	?INFO("~p -- decoded:~p",[?MODULE,Buf]),
	case rtsp_unpacket:decode(Buf) of
		{more,Rest} ->
			{noreply,State#state{buf=Rest}};
		{ok, {request, _Method, _URL, _Headers, _Body} = Request, Rest} ->
			?INFO("~p -- decoded url:~p",[?MODULE,Buf]),
			{ok, NewState} = handle_request(Request,Socket,State),
			handle_packet(NewState#state{buf=Rest})

	end.

%%----------------------------------------------------------------------
%% @spec 
%%----------------------------------------------------------------------

handle_request({request, Method, URL, Headers, _Body}, Socket,State) ->
	case Method of
		'OPTIONS' ->
			handle_options(Headers, Socket,State);
		'DESCRIBE' ->
			handle_describe(Headers, URL, Socket,State);
		'SETUP' ->
			handle_setup(Headers, URL, Socket,State);
		'PLAY' ->
			handle_play(Headers, Socket,State);
		'PAUSE' ->
			handle_pause(Headers, Socket,State);
		'TEARDOWN' ->
			handle_teardown(Headers, Socket,State);
		_ ->
			handle_not_supported(Headers, Socket,State)
	end.

%%----------------------------------------------------------------------
%% @spec 
%%----------------------------------------------------------------------

handle_options(Headers, Socket,State) ->
	Respon = rtsp_response_wrap:handle_reply(options, Headers),
	ok = gen_tcp:send(Socket, Respon),
	{ok, State}.

%%----------------------------------------------------------------------
%% @spec 
%%----------------------------------------------------------------------

handle_describe(Headers, URL, Socket,#state{rtp_pid=RtpPid} = State) ->
	case RtpPid of
		undefined ->
			{ok,Pid} = rtp_server:start_link(URL),
			{ok,SDP,SessionId} = rtp_server:get_sdp(Pid,URL),
			Respon = rtsp_response_wrap:handle_reply(describe, {Headers, SDP, URL}),
			ok = gen_tcp:send(Socket, Respon),
			{ok, State#state{rtp_pid=Pid,session=SessionId}};
		_ ->
			{ok,SDP,SessionId} = rtp_server:get_sdp(RtpPid,URL),
			Respon = rtsp_response_wrap:handle_reply(describe, {Headers,SDP, URL}),
			ok = gen_tcp:send(Socket, Respon),
			{ok, State#state{session=SessionId}}
	end.

%%----------------------------------------------------------------------
%% @spec 
%%----------------------------------------------------------------------
handle_setup(Headers, URL, Socket, State=#state{rtp_pid=RtpPid,session=Session}) ->
	{'Transport', Transport} = lists:keyfind('Transport', 1, Headers),
	case proplists:get_value(proto, Transport,{udp,"RTP/AVP"}) of
		{udp,TotalProtoString} -> 
			{'client_port', {Client_port_rtsp, Client_port_rtp}} = lists:keyfind('client_port', 1, Transport),
			Destination = proplists:get_value('destination', Transport, inet_parse:ntoa(State#state.ip)),
			{ok, {Address, _}} = inet:sockname(Socket),
			Source = inet_parse:ntoa(Address),
			case rtp_server:setup(RtpPid,#setup{client_port={Client_port_rtsp, Client_port_rtp},transport=udp,client_pid=self(),
										  destination=Destination,url=URL,seq=?DEFAULT_RTSP_SEQ_START}) of
				{ok,{Rtp_Port,Rtcp_Port}} ->
%% 					?INFO("~p -- return:~p",[?MODULE,{Rtp_Port,Rtcp_Port}]),
					Respon = rtsp_response_wrap:handle_reply(setup, {Headers,Source,TotalProtoString,
															    Rtp_Port,Rtcp_Port, 
															    Session,inet_parse:ntoa(State#state.ip)}),
					NewState = State#state{url=URL};
				{error,cannot_open_file} ->
					Respon = rtsp_response_wrap:handle_reply(cannot_find, Headers),
					NewState = State;
				{error,_} ->
					Respon = rtsp_response_wrap:handle_reply(server_internal_error, Headers),
				    NewState = State
			end;
		_ ->
			Respon = rtsp_response_wrap:handle_reply(unsupported_transport, Headers),
			NewState = State
	end,
	ok = gen_tcp:send(Socket, Respon),
	{ok, NewState}.

%%----------------------------------------------------------------------
%% @spec play scale 0.125-8 ########
%%----------------------------------------------------------------------
handle_play(Headers, Socket,#state{session=SeflSession} = State) ->
	{'Session', Session} = lists:keyfind('Session', 1, Headers),
	Respon = 
		case list_to_binary(SeflSession) of
			Session ->
				Range =
					case proplists:get_value('Range', Headers) of
						undefined ->
							undefined;
						Range0 ->
							{ok, BTime, ETime} = rtsp_unpacket:decode_range_value(Range0),
							{BTime, ETime}
					end,
				Scale =
					case proplists:get_value('Scale', Headers) of
						undefined ->
							?DEFAULT_SCAL;
						Scale0 ->
							?BINARY_TO_FLOAT(Scale0)
					end,
				rtp_server:play(State#state.rtp_pid, #play{range=Range,scale=Scale}),
				rtsp_response_wrap:handle_reply(play, Headers);
			_ ->
				rtsp_response_wrap:handle_reply(session_not_find, Headers)
		end,
	ok = gen_tcp:send(Socket, Respon),
	{ok, State}.
	

handle_pause(Headers, Socket,#state{session=SelfSession} = State) ->
	{'Session', Session} = lists:keyfind('Session',1,Headers),
	Respon = case Session =:= (list_to_binary(SelfSession)) of
		true ->
			Range1 = case lists:keyfind('Range', 1, Headers) of
						false ->
					 		undefined;
				 		{'Range', Range} ->
					 		{ok, BTime,_} = rtsp_unpacket:decode_range_value(Range),
					 		BTime
				end,
			rtp_server:pause(State#state.rtp_pid, #pause{range=Range1}),
			rtsp_response_wrap:handle_reply(normal_reply, Headers);
		false ->
			rtsp_response_wrap:handle_reply(session_not_find, Headers)
	end,
	ok = gen_tcp:send(Socket, Respon),
	{ok, State}.

handle_teardown(Headers, Socket, #state{session=SelfSession} = State) ->
	{'Session', Session} = lists:keyfind('Session',1,Headers),
	Respon = case Session =:= (list_to_binary(SelfSession)) of
				 true ->
%% 					rtp_server:close(State#state.rtp_pid),
					rtp_server:teardown(State#state.rtp_pid),
					NewState = State#state{rtp_pid=undefined},
					
					rtsp_response_wrap:handle_reply(normal_reply, Headers);
				 false ->
					 NewState = State,
					 rtsp_response_wrap:handle_reply(session_not_find, Headers)
			 end,
	gen_tcp:send(Socket, Respon),
	{ok, NewState}.
%%----------------------------------------------------------------------
%% @spec 
%%----------------------------------------------------------------------

handle_not_supported(Headers, Socket,State) ->
	Respon = rtsp_response_wrap:handle_reply(not_support_method,Headers),
	ok = gen_tcp:send(Socket, Respon),
	{ok, State}.



