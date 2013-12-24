%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2013-12-23
%%% -------------------------------------------------------------------
-module(rtp_stream_udp_sender).
-behaviour(rtp_sender).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rtp_session.hrl").
-include("rtsp.hrl").
-include("log.hrl").
%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([start_link/0,setup/2,stop/1,send/2,send_bye/1]).
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
    {ok, #rtp_stream_udp_sender{}}.

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
handle_call({setup,#setup{client_pid=ClientPid,seq=Seq, ssrc=SSRC, client_port={RtpPort,RtcpPort},destination=Destination}=Setup},_,
			#rtp_stream_udp_sender{senders=Senders} = State) ->
	case proplists:get_value(ClientPid, Senders) of
		undefined ->
			{ok, {{MyRtpPort , MyRtcpPort}=ServerPorts, {RtpSocket, RtcpSocket}}} = rtsp_util:open_groupsocket(self()),
			{ok, RtcpPid} = rtcp_socket:rtcp_start(MyRtcpPort,Destination,RtcpPort,RtcpSocket),
			ok = gen_udp:controlling_process(RtcpSocket, RtcpPid),
			{ok,SendDes} = inet:getaddr(Destination,inet),
			RtpState = #rtp_state{seq=Seq,ssrc=SSRC},
			Sender = #rtp_udp_sender{rtcp_socket=RtcpSocket,server_rtcp_port=MyRtcpPort,server_rtp_port=MyRtpPort,
															  destination=SendDes,rtcp_pid=RtcpPid,client_rtp_port=RtpPort,
											   				  rtp_socket=RtpSocket,rtp_state=RtpState,setup=true},
			?TRACK("~p -- setup for client:~p",[?MODULE,ClientPid]),
			{reply, {ok,ServerPorts}, State#rtp_stream_udp_sender{senders=[{ClientPid,Sender}|Senders]}};
		#rtp_udp_sender{server_rtp_port=MyRtpPort,server_rtcp_port=MyRtcpPort} ->
			{reply, {ok,{MyRtpPort,MyRtcpPort}}, State}
	end;

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc
%% 		Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
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
handle_info(Info, State) ->
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
terminate(Reason, State) ->
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

