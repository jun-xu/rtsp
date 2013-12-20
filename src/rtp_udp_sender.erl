%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2013-5-27
%%% -------------------------------------------------------------------
-module(rtp_udp_sender).
-behaviour(rtp_sender).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rtp_session.hrl").
-include("rtsp.hrl").
-include("log.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,setup/2,stop/1,send/2,send_bye/1]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link(?MODULE, [], []).

setup(Pid,Setup) ->
	gen_server:call(Pid, {setup,Setup},infinity).

send_bye(Pid) ->
	gen_server:cast(Pid, send_bye).

send(Pid,Frame) ->
	gen_server:cast(Pid, {send,Frame}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

%% ====================================================================
%% Server functions
%% ====================================================================

%%----------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc
%%      Initializes the server
%% @end
%%----------------------------------------------------------------------
init([]) ->
    {ok, #rtp_udp_sender{}}.

%%--------------------------------------------------------------------
%% @private
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @doc
%% 		Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({setup,Setup}, _From, #rtp_udp_sender{setup=true,server_rtp_port=MyRtpPort,server_rtcp_port=MyRtcpPort} = State) ->
	?INFO("~p -- already setup:~p",[?MODULE,Setup]),
	{reply, {ok,{MyRtpPort,MyRtcpPort}}, State};
handle_call({setup,#setup{seq=Seq, ssrc=SSRC, client_port={RtpPort,RtcpPort},destination=Destination} = Setup}, _From, State) ->
	?INFO("~p -- setup:~p~n",[?MODULE,Setup]),
	{ok, {{MyRtpPort , MyRtcpPort}=ServerPorts, {RtpSocket, RtcpSocket}}} = rtsp_util:open_groupsocket(self()),
	{ok, RtcpPid} = rtcp_socket:rtcp_start(MyRtcpPort,Destination,RtcpPort,RtcpSocket),
	ok = gen_udp:controlling_process(RtcpSocket, RtcpPid),
	{ok,SendDes} = inet:getaddr(Destination,inet),
	RtpState = #rtp_state{seq=Seq,ssrc=SSRC},
	{reply, {ok,ServerPorts}, State#rtp_udp_sender{rtcp_socket=RtcpSocket,server_rtcp_port=MyRtcpPort,server_rtp_port=MyRtpPort,
															  destination=SendDes,rtcp_pid=RtcpPid,client_rtp_port=RtpPort,
											   				  rtp_socket=RtpSocket,rtp_state=RtpState,setup=true}};


handle_call(Request, _From, State) ->
    ?INFO("~p -- no implement call:~p~n",[?MODULE,Request]),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc
%% 		Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({send,#frame{type=FrameType,timestamp=_CurrentPos} = Frame},#rtp_udp_sender{setup=true,destination=SendDes,client_rtp_port=RtpPort,
															  rtp_socket=RtpSocket,rtp_state=RtpState} = State) ->
%% 	?DEBUG("~p -- send frame:~p~n",[?MODULE,_CurrentPos]),
	{ok, Rtps, NewRtpState} = 
		case FrameType of
			h264 ->
				h264_rtp_wrap:wrap(Frame, RtpState);
			"H264" ->
				h264_rtp_wrap:wrap(Frame, RtpState);
			"h264" ->
				h264_rtp_wrap:wrap(Frame, RtpState);
			aac ->
				adts_rtp_wrap:wrap(Frame, RtpState);
			_ ->
				default_rtp_wrap:wrap(Frame, RtpState)
		end,
	lists:foreach(fun(Rtp) -> 
%% 						?DEBUG("~p -- send frame:~p to ~p:~p~n",[?MODULE,{_CurrentPos,FrameType},SendDes,RtpPort]),
						loop_send_rtp(RtpSocket, SendDes, RtpPort, Rtp)
				end,Rtps),
	{noreply, State#rtp_udp_sender{rtp_state=NewRtpState}};
%% 	{noreply, State};

handle_cast(send_bye,#rtp_udp_sender{setup=true,rtcp_pid=RtcpPid} = State) ->
	case RtcpPid of
		undefined -> ok;
		_ -> rtcp_socket:rtcp_stop(RtcpPid)
	end,
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc
%% 		Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(Reason,#rtp_udp_sender{rtcp_pid=RtcpPid,rtp_socket=Socket}) ->
	?INFO("~p -- terminate by reason:~p~n",[?MODULE,Reason]),
	case Socket of
		undefined -> ok;
		_ ->
			catch gen_udp:close(Socket)
	end,
	case RtcpPid of
		undefined -> ok;
		_ ->
			catch rtcp_socket:rtcp_stop(RtcpPid),
			catch rtcp_socket:stop(RtcpPid)
	end,
    ok.

%%-------------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%%-------------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
loop_send_rtp(RtpSocket,Destination,RtpPort,Rtp) ->
  case prim_inet:sendto(RtpSocket,Destination,RtpPort,Rtp) of
%%   case gen_udp:send(RtpSocket,Destination,RtpPort,Rtp) of
		ok -> ok;
		{error,eagain} ->
			 loop_send_rtp(RtpSocket,Destination,RtpPort,Rtp);
		E ->
			?DEBUG("~p -- send frame to ~p:~p error:~p~n",[?MODULE,Destination,RtpPort,E]),
			E  
 end.
