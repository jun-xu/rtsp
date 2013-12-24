%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2013-5-27
%%% -------------------------------------------------------------------
-module(rtp_session_ex).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rtp_session.hrl").
-include("sdp.hrl").
-include("media_info.hrl").
-include("rtsp.hrl").
-include("video_file.hrl").
-include("log.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,setup/2,play/2,pause/2,get_sdp/2]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BEFORE_SEND_FIRST_FRAME_TIME,40).
-define(DEFAULT_MIN_SEND_INTERVAL,0).
-define(DEFAULT_JUMP_SEND_INTERVAL,40).
-define(DEFAULT_MAX_DELAY_TIME,3000).
-define(DEFAULT_Min_CACHE_FRAMES_SIZE,40).
-define(DEFAULT_RETRY_TIME,1000).



%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec setup(Pid::pid(), Setup::#setup{}) -> {ok,ServerPort::{0..65536,0..65536}} | {error,Reason::atom()}.
setup(Pid,Setup) ->
	gen_server:call(Pid, Setup,infinity).

play(Pid,Play) ->
	gen_server:call(Pid, Play,infinity).

pause(Pid,Pause) ->
	gen_server:call(Pid, Pause,infinity).

-spec get_sdp(Pid::pid(),URL::string()|binary()) -> {ok, SDP::string()} | error.
get_sdp(Pid,URL) ->
	gen_server:call(Pid, {get_sdp,URL},infinity).

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
    {ok, #rtp_session_state_ex{}}.

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

handle_call({get_sdp,URL},_,#rtp_session_state_ex{sdp=undefined} = State) ->
	?INFO("~p -- get sdp of url:~p",[?MODULE,URL]),
	case decode_file_path(URL) of
		{ok,FilePath} ->
			case rtsp_util:choose_file_reader(FilePath) of
				undefined ->
					?INFO("~p -- unknow file:~p",[?MODULE,FilePath]),
					{stop, close, {error, cannot_find}, State};
				ReadMod ->
					case root_dir_manager:get_file_path(FilePath) of
						{ok,RelFilePath} ->
							{ok,ReaderPid} = ReadMod:start_link(),
							case ReadMod:init_file(ReaderPid,RelFilePath) of
								ok	->
									case ReadMod:get_sdp(ReaderPid) of
										{ok,#media_info{options=Options} =MediaInfo} ->
											{ok,SessionId,NewMediaInfo} = rtsp_util:gen_sdp_session(MediaInfo#media_info{options=[{url,URL}|Options]}),
											Sdp = sdp:encode(NewMediaInfo),
											?INFO("~p -- init sdp:~p~nsession:~p",[?MODULE,Sdp,SessionId]),
											{reply, {ok,Sdp,SessionId}, State#rtp_session_state_ex{sdp=Sdp,session_id=SessionId,media_info=NewMediaInfo,source_mod=ReadMod,source_pid=ReaderPid}};
										E ->
											?INFO("~p -- get sdp ~p error:~p",[?MODULE,RelFilePath,E]),
											{stop, close, {error, cannot_find}, State}
									end;
								E ->
									?INFO("~p -- init ~p error:~p",[?MODULE,RelFilePath,E]),
									{stop, close, {error, cannot_find}, State}
							end;
						{error,_Reason} ->
							{stop, close, {error, cannot_find}, State}
					end
			end;
		{error,Reason} ->
			?INFO("~p -- bad request:~p by reason:~p",[?MODULE,URL,Reason]),
			{stop, close, {error, bad_request}, State}
	end;
handle_call({get_sdp,_URL},_,#rtp_session_state_ex{sdp=Sdp,session_id=SessionId} = State) ->
	{reply, {ok,Sdp,SessionId}, State};
%% 	{reply, ok, State};
handle_call(#setup{url=Url} = Setup, _From, #rtp_session_state_ex{channels=Channels} = State) ->
	?INFO("~p -- udp setup:~p~n",[?MODULE,Setup]),
	case parse_track(Url) of
		{error,not_found} -> 
			{reply, {error, bad_request}, State};
		{ok,Track} ->
			case proplists:get_value(Track, Channels) of
				{Mod,SenderPid} ->
					case Mod:setup(SenderPid,Setup) of
						{ok,ServerPorts} -> {reply,{ok,ServerPorts},State};
						{error,Reason} ->
							?INFO("~p -- setup channel of track:~p error by reason:~p",[?MODULE,Track,Reason]),
							{reply, {error, bad_request}, State}
					end;
				undefined ->
					Mod = rtp_sender:get_transport_mod(?RTP_AVP),
					{ok,SenderPid} = Mod:start_link(),
					case Mod:setup(SenderPid,Setup) of
						{ok,ServerPorts} -> 
							{reply,{ok,ServerPorts},State#rtp_session_state_ex{channels=[{Track,{Mod,SenderPid}}|Channels]}};
						{error,Reason} ->
							?INFO("~p -- setup channel of track:~p error by reason:~p",[?MODULE,Track,Reason]),
							{reply, {error, bad_request}, State}
					end
			end
	end;
	

%% source not setup. return 404 error.
handle_call(#play{}, _From, #rtp_session_state_ex{source_pid=undefined} = State) ->
	?INFO("~p -- source is not setup.~n",[?MODULE]),
	{reply, {error,cannot_find}, State};
%% when is playing, just dump to play list.
%% if play.range.startTime is undefined,but endtime isnot undefined, the play is invalid.  457 error.
handle_call(#play{range={undefined,EndTime}} = Play, _From, State) when EndTime =/= undefined->
	?INFO("~p -- bad play range:~p~n",[?MODULE,Play]),
	{reply, {error,bad_play_range}, State};
% dump to play list and wait.
handle_call(#play{} = Play, _From, #rtp_session_state_ex{plays=Plays,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	{reply, ok, State#rtp_session_state_ex{plays=lists:reverse([Play|lists:reverse(Plays)])}};

%% receive play cmd which range is undefined or {undefined,undefined} continue to play.
handle_call(#play{range=undefined,scale=Scale}=P, _From, #rtp_session_state_ex{current_play=#play{}=CPlay,rtp_state=?RTP_SESSION_STATE_PAUSING} = State) ->
	self() ! send_frame,
	NewScale = check_scale(Scale),
	CFrameSize = round(rtsp_util:get_app_env(cache_frames_size, [], ?DEFAULT_Min_CACHE_FRAMES_SIZE+10) * NewScale),
	?INFO("~p -- continue to play:~p cache frame size:~p~n",[?MODULE,P,CFrameSize]),
	{reply, ok, State#rtp_session_state_ex{cache_frame_size=CFrameSize,current_play=CPlay#play{scale=NewScale},rtp_state=?RTP_SESSION_STATE_PLAYING}};
	
handle_call(#play{range={undefined,undefined},scale=Scale}=P, _From, #rtp_session_state_ex{current_play=#play{}=CPlay,rtp_state=?RTP_SESSION_STATE_PAUSING} = State) ->
	self() ! send_frame,
	NewScale = check_scale(Scale),
	CFrameSize = round(rtsp_util:get_app_env(cache_frames_size, [], ?DEFAULT_Min_CACHE_FRAMES_SIZE+10) * NewScale),
	?INFO("~p -- continue to play:~p cache frame size:~p~n",[?MODULE,P,CFrameSize]),
	{reply, ok, State#rtp_session_state_ex{cache_frame_size=CFrameSize,current_play=CPlay#play{scale=NewScale},rtp_state=?RTP_SESSION_STATE_PLAYING}};

%% other, dump to play list and start a new play.
handle_call(#play{} = Play, _From, #rtp_session_state_ex{plays=Plays,rtp_state=?RTP_SESSION_STATE_PAUSING} = State) ->
	self() ! play_next,
	{reply, ok, State#rtp_session_state_ex{current_play=undefined,plays=lists:reverse([Play|lists:reverse(Plays)])}};

handle_call(#pause{} = Pause,_,#rtp_session_state_ex{send_timer=Timer} = State) ->
	cancel_timer(Timer),
	?INFO("~p -- pause:~p~n",[?MODULE,Pause]),
	%% clean all waited plays when pause. 
	{reply, ok, State#rtp_session_state_ex{pre_frame=undefined,send_timer=undefined,plays=[],rtp_state=?RTP_SESSION_STATE_PAUSING}};

handle_call(close, _From, State) ->
	{stop, normal, ok, State};

handle_call(teardown, _From,State) ->
	?INFO("~p -- teardown.~n",[?MODULE]),
	{stop, normal, ok, State};

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
handle_info({data, {ReadId,{error,retry}}},#rtp_session_state_ex{source_mod=ReaderMod,read_id=ReadId,source_pid=Pid,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	?ERROR("~p -- read data error:~p...~n",[?MODULE,retry]),
	IsReading = retry_next_read(false, Pid, ReadId,ReaderMod),
	{noreply, State#rtp_session_state_ex{is_reading=IsReading}};
  
handle_info({data, {ReadId,{error,Reason}}},#rtp_session_state_ex{read_id=ReadId,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	?ERROR("~p -- read data error:~p,terminate...~n",[?MODULE,Reason]),
	{stop, normal, State};


handle_info({data, {ReadId,{error,Reason}}},#rtp_session_state_ex{read_id=ReadId,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	?ERROR("~p -- read data error:~p,terminate...~n",[?MODULE,Reason]),
	{stop, normal, State};
%% if rtp state is playing,but frames list empty, active send frame!.
handle_info({data, {ReadId,Frames}},#rtp_session_state_ex{frames=[],cache_frame_size=CacheSize,source_pid=Pid,source_mod=ReaderMod,
														  read_id=ReadId,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	?DEBUG("~p -- read frames and start send.~n",[?MODULE]),
	self() ! send_frame,
	IsReading = check_next_read(false, Frames, CacheSize, Pid, ReadId,ReaderMod),
	{noreply, State#rtp_session_state_ex{frames=Frames,is_reading=IsReading}};
%% if rtp state is playing and frames list isnot empty,dump to frames list to send auto!.
handle_info({data, {ReadId,NewFrames}},#rtp_session_state_ex{frames=Frames,cache_frame_size=CacheSize,source_pid=Pid,source_mod=ReaderMod,
															 read_id=ReadId,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	Frames0 = mergin_frames(NewFrames,lists:reverse(Frames)),
	IsReading = check_next_read(false, Frames0, CacheSize, Pid, ReadId,ReaderMod),
	{noreply, State#rtp_session_state_ex{frames=Frames0,is_reading=IsReading}};
%% if rtp state isnot playing,just dump to frames list.
handle_info({data, {ReadId,NewFrames}},#rtp_session_state_ex{frames=Frames,read_id=ReadId} = State) ->
	Frames0 = mergin_frames(NewFrames,lists:reverse(Frames)),
	{noreply, State#rtp_session_state_ex{frames=Frames0,is_reading=false}};
handle_info({data, {ReadId,_}},State) ->
	?INFO("~p -- ignore read data with id:~p~n",[?MODULE,ReadId]),																			  
	{noreply, State};


%% send frame but frames list is empty.
handle_info(send_frame,#rtp_session_state_ex{frames=[],send_timer=Timer,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	?INFO("~p -- send frames empty!.~n",[?MODULE]),
	cancel_timer(Timer),
	{noreply, State#rtp_session_state_ex{send_timer=undefined}};
%% send data to client.
handle_info(send_frame,#rtp_session_state_ex{current_play=CurrentPlay,send_timer=Timer,frames=[eof],rtp_state=?RTP_SESSION_STATE_PLAYING}=State) ->
	?INFO("~p -- play:~p over.~n",[?MODULE,CurrentPlay]),
	cancel_timer(Timer),
	self() ! play_next,
	{noreply, State#rtp_session_state_ex{send_timer=undefined,frames=[],pre_frame=undefined,current_play=undefined,rtp_state=?RTP_SESSION_STATE_PAUSING}};
%% when read error ,stop session.
handle_info(send_frame,#rtp_session_state_ex{current_play=CurrentPlay,send_timer=Timer,frames=[error],rtp_state=?RTP_SESSION_STATE_PLAYING}=State) ->
	?INFO("~p -- play:~p error and stop.~n",[?MODULE,CurrentPlay]),
	cancel_timer(Timer),
	{stop, error, State#rtp_session_state_ex{send_timer=undefined,frames=[],pre_frame=undefined,current_play=undefined,rtp_state=?RTP_SESSION_STATE_PAUSING}};

handle_info(send_frame,#rtp_session_state_ex{source_mod=ReaderMod,cache_frame_size=CacheSize,is_reading=Reading,frames=[#frame{track=Track,timestamp=CurrentPos} = Frame|RestFrames],
											 pre_frame=PreFrame,channels=Channels,send_timer=Timer,read_id=ReadId,source_pid=Pid,
											 current_play=#play{scale=Scale}=CurrentPlay,rtp_state=?RTP_SESSION_STATE_PLAYING} = State) ->
	cancel_timer(Timer),
%% 	?DEBUG("~p -- send frame:~p of track:~p at ~p.~n",[?MODULE,CurrentPos,Track,rtsp_util:now_in_millisecond()]),
	case proplists:get_value(Track, Channels) of
		undefined -> ok;
		{Mod,Sender} ->
%% 			?DEBUG("~p -- send frame:~p of track:~p at ~p to ~p.~n",[?MODULE,CurrentPos,Track,rtsp_util:now_in_millisecond(),Sender]),
			ok = Mod:send(Sender,Frame)
	end,
	IsReading = check_next_read(Reading, RestFrames, CacheSize, Pid, ReadId,ReaderMod),
	case RestFrames of
		[] ->  %% wait next frames
			?INFO("~p -- send frames empty!.~n",[?MODULE]),
			{noreply, State#rtp_session_state_ex{send_timer=undefined,frames=[],pre_frame=undefined,is_reading=IsReading}};
		[eof] ->  %% play over.
			?INFO("~p -- play:~p over.~n",[?MODULE,CurrentPlay]),
			self() ! play_next,
			{noreply, State#rtp_session_state_ex{send_timer=undefined,frames=[],pre_frame=undefined,current_play=undefined,
												 rtp_state=?RTP_SESSION_STATE_PAUSING,is_reading=false}};
		[error] ->
			{stop,error, State#rtp_session_state_ex{send_timer=undefined,frames=[],pre_frame=undefined,current_play=undefined,
												 rtp_state=?RTP_SESSION_STATE_PAUSING,is_reading=false}};
		[#frame{timestamp=NextPos}|_] ->
			Now = rtsp_util:now_in_millisecond(),
			{CompensationTime,DelayTime} = case PreFrame of
							undefined -> 
								{0,round((NextPos - CurrentPos) / Scale)};
							{PreTime,PrePos} -> 
%% 								?DEBUG("~p -- delay time:~p ~p~n",[?MODULE,round((NextPos - CurrentPos) / Scale - (Now - PreTime - (CurrentPos-PrePos)/ Scale)),
%% 																	 {NextPos,CurrentPos,Scale,Now,PreTime,PrePos}]),
								%% compemsate send delay.
								CompensationTime0  = Now - PreTime - (CurrentPos-PrePos)/ Scale,
								{CompensationTime0,round((NextPos - CurrentPos) / Scale - CompensationTime0)}
						end,
			RealFrameSendTime = Now - CompensationTime,  %% time after compensate!!
			case DelayTime > 0 of
				true ->
					RealMaxDelayTime = ?DEFAULT_MAX_DELAY_TIME / Scale,
					case DelayTime >  RealMaxDelayTime of
						true ->
							%% maybe some gap of souce file,jump to next frame 
							RealJumpIntervalTime = round(?DEFAULT_JUMP_SEND_INTERVAL/ Scale),
							NewSendTimer = timer:send_after(RealJumpIntervalTime, send_frame),
							JumpTimeToPos = NextPos - ?DEFAULT_JUMP_SEND_INTERVAL,
							?INFO("~p -- jump frame pos from ~p to ~p,delay:~p~n",[?MODULE,CurrentPos,NextPos,RealJumpIntervalTime]),
							{noreply, State#rtp_session_state_ex{send_timer=NewSendTimer,frames=RestFrames,pre_frame={RealFrameSendTime,JumpTimeToPos},is_reading=IsReading}};
						false ->
%% 							?DEBUG("~p -- send delay:~p~n",[?MODULE,DelayTime]),
							NewSendTimer = timer:send_after(DelayTime, send_frame),
							{noreply, State#rtp_session_state_ex{send_timer=NewSendTimer,frames=RestFrames,pre_frame={RealFrameSendTime,CurrentPos},is_reading=IsReading}}
					end;
					
				false ->
					NewSendTimer = timer:send_after(?DEFAULT_MIN_SEND_INTERVAL, send_frame),
					{noreply, State#rtp_session_state_ex{send_timer=NewSendTimer,frames=RestFrames,pre_frame={RealFrameSendTime,CurrentPos},is_reading=IsReading}}
			end
	end;
%% if current stat isnot playing, ignore.
handle_info(send_frame,#rtp_session_state_ex{send_timer=Timer} = State) ->
	cancel_timer(Timer),
%% 	?DEBUG("~p -- send farme not play:~p!~n",[?MODULE,State]),
	{noreply, State#rtp_session_state_ex{send_timer=undefined}};

%% when has not play cmd,send by to sender.
handle_info(play_next,#rtp_session_state_ex{current_play=undefined,channels=Channels,rtp_state=?RTP_SESSION_STATE_PAUSING,plays=[]} = State) ->
	%% send by to sender.
	loop_send_bye(Channels),
	{noreply, State};

%% start a new play.
handle_info(play_next,#rtp_session_state_ex{current_play=undefined,source_mod=ReaderMod,rtp_state=?RTP_SESSION_STATE_PAUSING,send_timer=Timer,source_pid=Pid,
											read_id=ReadId,plays=[#play{scale=Scale,range=Range} = Play|Rest]} = State) ->
	{StartTime, _EndTime} = case Range of
								undefined -> {0,undefined};
							    {undefined,undefined} -> {0,undefined};
							    {S,E} -> {S,E}
						   end,
	cancel_timer(Timer),
	ok = ReaderMod:position(Pid, StartTime, 0),
	NewReadId = ReadId + 1,
	ok = ReaderMod:read(Pid, self(), NewReadId),
	NewScale = case Scale of
		undefined -> 1.0;
		_ -> Scale
	end,
	CFrameSize = round(rtsp_util:get_app_env(cache_frames_size, [], ?DEFAULT_Min_CACHE_FRAMES_SIZE+10) * NewScale),
	CacheFrameSize = case CFrameSize < ?DEFAULT_Min_CACHE_FRAMES_SIZE of
		true ->
			 ?DEFAULT_Min_CACHE_FRAMES_SIZE;
		false ->
			CFrameSize
	end,
	?INFO("~p --start play:~p cache frame size:~p~n",[?MODULE,Play,CacheFrameSize]),
	{noreply, State#rtp_session_state_ex{current_play=Play#play{scale=NewScale},send_timer=undefined,frames=[],pre_frame=undefined,plays=Rest,
										   read_id=NewReadId,rtp_state=?RTP_SESSION_STATE_PLAYING,cache_frame_size=CacheFrameSize}};

handle_info(Info, State) ->
	?INFO("~p -- not implement info:~p~n",[?MODULE,Info]),
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
terminate(Reason,#rtp_session_state_ex{send_timer=SendTimer,source_mod=ReaderMod,source_pid=Reader,channels=Channels}) ->
	?INFO("~p -- terminate by reason:~p~n",[?MODULE,Reason]),
	cancel_timer(SendTimer),
	case Reader of
		undefined -> ok;
		_ ->
			catch ReaderMod:stop(Reader)
	end,
	
	stop_sender(Channels),
	
    ok.

%%-------------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
cancel_timer(undefined) -> ok;
cancel_timer(Timer) ->
	timer:cancel(Timer).

%% mergin frist list to second list.
mergin_frames([],L) -> lists:reverse(L);
mergin_frames([H|T],L) ->
	mergin_frames(T, [H|L]).


stop_sender([]) -> ok;
stop_sender([{_,{Mod,Pid}}|T]) ->
	catch Mod:stop(Pid),
	stop_sender(T).
	

loop_send_bye([]) -> ok;
loop_send_bye([{_,{Mod,Sender}}|T]) ->
	catch Mod:send_bye(Sender),
	loop_send_bye(T).

retry_next_read(Reading,SourcePid,ReadId,ReaderMod) ->
	case Reading of
		true -> true;
		false ->
			Self = self(),
			timer:apply_after(?DEFAULT_RETRY_TIME, ReaderMod, read,[SourcePid, Self, ReadId]),
			true
	end.
	
check_next_read(Reading,RestFrames,CacheSize,SourcePid,ReadId,ReaderMod) ->
	case Reading of
		true -> true;
		false ->
			case length(RestFrames) < CacheSize of
				true ->
					case lists:reverse(RestFrames) of
						[error|_] -> false;
						[eof|_] -> false;
						_ ->
%% 							?INFO("~p -- read next data current size:~p.~n",[?MODULE,length(RestFrames)]),
							ok = ReaderMod:read(SourcePid, self(), ReadId),
							true
					end;
				false -> false
			end
	end.

check_scale(undefined) -> ?DEFAULT_SCAL;
check_scale(Scale) -> Scale.

decode_file_path(Url) ->
	case re:run(Url, ".*"++ ?VIDEO_PRE_FIX ++"(.*)", [{capture, all_but_first, list}]) of
		nomatch -> {error,nomatch};
		{match,[FilePath]} ->
			{ok,FilePath}
	end.

parse_track(Url) ->
	case re:run(Url, ".*"++ ?VIDEO_PRE_FIX ++".*/"++?TRACK_TAG++"=(.*)", [{capture, all_but_first, list}]) of
		nomatch -> {error,not_found};
		{match,[Track]} ->
			{ok,list_to_integer(Track)}
	end.
