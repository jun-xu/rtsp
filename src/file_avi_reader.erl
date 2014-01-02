%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2013-5-27
%%% -------------------------------------------------------------------
-module(file_avi_reader).

-behaviour(gen_server).
-behaviour(rtsp_source_reader).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").
-include("h264.hrl").
-include("media_info.hrl").
-include("rtsp.hrl").
-include("rtp_session.hrl").
-include("video_file.hrl").
-include("video_avi.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-ifdef(TEST).
-compile(export_all).
-endif.

-export([start_link/0,init_file/2,position/3,stop/1,read/3,get_state/1,
		 get_sdp/1,pause/1]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link(?MODULE, [], []).

init_file(Pid,FilePath) ->
	gen_server:call(Pid, {init,FilePath},infinity).

get_sdp(Pid) ->
	gen_server:call(Pid, get_sdp,infinity).

pause(_Pid) -> ok.		%% no use.

position(Pid,StartPos,Track) ->
	gen_server:call(Pid, {position,StartPos,Track},infinity).

read(Pid,From,ReadId) ->
	gen_server:cast(Pid, {read,From,ReadId}).

get_state(Pid) ->
	gen_server:call(Pid,get_state).

stop(Pid) ->
	gen_server:call(Pid, stop,infinity).

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
    {ok, #state{}}.

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

handle_call({init,FilePath},_,State) ->
	case prim_file:open(FilePath,?FILE_READ_OPEN_OPTIONS) of
		{ok,FD} ->
			{reply, ok, State#state{file_path=FilePath,fd=FD}};	
		{error,Reason} ->
			?INFO("~p -- open file:~p error by reason:~p",[?MODULE,FilePath,Reason]),
			{error,Reason}
	end;

handle_call(get_sdp,_,#state{fd=FD,avi_info=undefined} = State) ->
	{ok,#media_info{metadata=#stream_info{codec=Fcc,params=#video_params{fps=FPS}}}=MediaInfo,IVS,StartOffset,_RelDataSize} = decode_avi_file(FD),
	IndexOffsetType = analyse_index_offset_type(IVS,StartOffset),
	?DEBUG("~p -- avi info:~p~nfcchandler:~p,startoffset:~p,indexOffsetType:~p",[?MODULE,MediaInfo,Fcc,StartOffset,IndexOffsetType]),
%% 	Flags = gen_all_frame_flag(MediaInfo),
	ReadHeader = #avi_read_header{vids_fcc_handler=Fcc,fps=(1000 div FPS),vids_account=0},
	{ok,NewMediaInfo} = case IVS of
						[] ->
							read_first_frame(FD,StartOffset+4,MediaInfo);
						[#track_info{iindexs=[]}|_] ->
							read_first_frame(FD,StartOffset+4,MediaInfo);
						[#track_info{iindexs=[{_,Offset,_}|_]}|_] ->
							NewOffset = count_new_offset(StartOffset,Offset,IndexOffsetType),
							read_first_frame(FD,NewOffset,MediaInfo)
					end,
	{ok,_} = prim_file:position(FD, StartOffset),
	{reply, {ok,NewMediaInfo}, State#state{avi_index_offset_type=IndexOffsetType,avi_read_header=ReadHeader,avi_info=NewMediaInfo,i_video_indexs=IVS,
									cur_offset=StartOffset,data_start_offset=StartOffset}};	
handle_call(get_sdp,_,#state{avi_info=MediaInfo} = State) ->
	{reply, {ok,MediaInfo}, State};

handle_call({position,_,_},_,#state{i_video_indexs=[]} = State) ->
	?INFO("~p -- no iindex to position.",[?MODULE]),
	{reply, ok, State#state{first_read=true}};
handle_call({position,0,Track},_,#state{fd=FD,avi_read_header=ReadHeader,data_start_offset=StartOffset,i_video_indexs=IVS,
									 avi_index_offset_type=IndexOffsetType} = State) ->
	case search_track(Track, IVS) of
		undefined -> 
			?INFO("~p -- no track to position",[?MODULE]),
			{reply, ok, State#state{first_read=true}};
		#track_info{iindexs=[]} ->
			?INFO("~p -- no track:~p iindex to position.",[?MODULE,Track]),
			{reply, ok, State#state{first_read=true}};
		#track_info{iindexs=[{Account,Offset,_}|_]} ->
			NewOffset = count_new_offset(StartOffset,Offset,IndexOffsetType),
			?INFO("~p -- setup from ~p to offset:~p.",[?MODULE,0,NewOffset]),
			{ok,_} = prim_file:position(FD, NewOffset),
			{reply, ok, State#state{read_start_pos=0,cur_offset=NewOffset,avi_read_header=ReadHeader#avi_read_header{vids_account=Account},
							buff= <<>>,first_read=true}}
	end;
%% 
	
handle_call({position,StartPos,Track},_,#state{avi_read_header=ReadHeader=#avi_read_header{fps=FPS},data_start_offset=StartOffset,
											 i_video_indexs=IVS,fd=FD,avi_index_offset_type=IndexOffsetType} = State) ->
	case search_track(Track, IVS) of
		undefined -> 
			?INFO("~p -- no track to position",[?MODULE]),
			{reply, ok, State#state{first_read=true}};
		#track_info{iindexs=[]} ->
			?INFO("~p -- no track:~p iindex to position.",[?MODULE,Track]),
			{reply, ok, State#state{first_read=true}};
		#track_info{iindexs=[{PreAccount,PreOffset,_}|T]} ->
			{Account,Offset} = search_iindexs(StartPos,FPS,{PreAccount,PreOffset},T),
			NewOffset = count_new_offset(StartOffset,Offset,IndexOffsetType),
			?INFO("~p -- setup from ~p to offset:~p.",[?MODULE,0,NewOffset]),
			{ok,_} = prim_file:position(FD, NewOffset),
			{reply, ok, State#state{read_start_pos=StartPos,cur_offset=NewOffset,avi_read_header=ReadHeader#avi_read_header{vids_account=Account},
									buff= <<>>,first_read=true}}
	end;
%% 	
	
handle_call(stop,_,State) ->
	{stop, normal,ok, State};

handle_call(Request, From, State) ->
	?INFO("~p -- no implement call:~p",[?MODULE,Request]),
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
handle_cast({read,From,ReadId},#state{fd=undefined} = State) ->
	ok = callback_to(From,ReadId,[eof]),
	{noreply, State};
handle_cast({read,From,ReadId},#state{avi_read_header=ReadHeader,first_read=FirstRead,fd=FD,buff = Buff} = State) ->
 	?DEBUG("~p -- read data:~p",[?MODULE,ReadHeader]),
	RealReadLength = case FirstRead of
					 true -> 
						 ?DEFAULT_MIN_READ_FRAMES_Length div 2;
					 false ->
						 ?DEFAULT_MIN_READ_FRAMES_Length
				 end,
	case read_frames(FD,From,ReadId,RealReadLength,Buff,ReadHeader) of
		{eof,NewInfo} ->
			{noreply, State#state{buff = <<>>,avi_read_header=NewInfo}};		
		{ok,NewReadHeader,Rest} ->
			{noreply, State#state{avi_read_header=NewReadHeader,first_read=false,buff=Rest}};
		{error,_Reason} ->
			{stop, normal,State}
	end;
	
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
terminate(Reason, #state{fd=FD} = State) ->
	?INFO("~p -- terminate by reason:~p",[?MODULE,Reason]),
	case FD of
		undefined -> ok;
		_ ->
			catch prim_file:close(FD)
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
callback_to(_,_,[]) -> ok;
callback_to(From,ReadId,Frames) ->
	From ! {data, {ReadId,lists:reverse(Frames)}},
	ok.

read_frames(_FD,_From,_ReadId,NextReadLength,Buff,Info) when NextReadLength =< 0 -> 
%% 	?INFO("~p-- restbin:~p",[?MODULE,Buff]),
	{ok,Info,Buff};
read_frames(FD,From,ReadId,ReadLength,Buff,Info) ->
	case prim_file:read(FD, ?DEFAULT_READ_FILE_BUFF_SIZE) of
		eof -> 
			?INFO("~p -- read file eof.",[?MODULE]),
			{eof,Info};
		{ok,Data} ->
			case loop_analyse_frames(From,ReadId,ReadLength,[],<<Buff/binary,Data/binary>>,Info) of
				{eof,NextInfo} -> 
					
					{eof,NextInfo};
				{ok,NextReadLength,NextInfo,Rest} ->
					?INFO("~p -- read NextReadLength:~p",[?MODULE,NextReadLength]),
					read_frames(FD,From,ReadId,NextReadLength,Rest,NextInfo);
				{error,Reason} ->
					{error,Reason}
			end;
		{error,Reason} ->
			{error,Reason}
	end.

decode_avi_file(FD) ->
	case prim_file:read(FD, 24) of
		{ok,<<?AVI_RIFF,_TotalSize:32/little,?AVI_FLG,?AVI_LIST,ListSize:32/little,?AVI_LIST_FLAG>>} ->
			read_avi_list(FD,ListSize - 4);
		{error,Reason} ->
			?INFO("~p -- read avi head error:~p",[?MODULE,Reason]),
			{error,decode_failed}
	end.

read_avi_list(FD,ListSize) ->
%% 	?INFO("~p -- read list size:~p",[?MODULE,Size]),
	case prim_file:read(FD, ListSize) of
		{ok,<<?AVI_HEAD,HeadSize:32/little,MetadataBin:HeadSize/binary,R/binary>>} ->
			<<DwMicroSecPerFrame:32/little,_:12/binary,DwTotalFrames:32/little,_:4/binary,DwStreams:32/little,_:4/binary,DwWidth:32/little,DwHeight:32/little,_/binary>> = MetadataBin,
			MSPerFrame = DwMicroSecPerFrame div 1000,
			FPS = 1000 div MSPerFrame,
			Duration = MSPerFrame * DwTotalFrames,
			MetadataInfo = #stream_info{content=?FRAME_CONTENT_METADATA,params=#video_params{fps=FPS,height=DwHeight,width=DwWidth}},
			MediaInfo = #media_info{duration=Duration,flow_type=?FLOW_TYPE_FILE,metadata=MetadataInfo},
			case loop_decode_lists(0,DwStreams,R,MediaInfo) of
				{ok,NNMediaInfo} ->
					decode_index(FD, NNMediaInfo);
				{error,_} = E ->
					E
			end;
		{error,Reason} ->
			?INFO("~p -- read avi totole list error:~p",[?MODULE,Reason]),
			{error,decode_failed}
	end.

loop_decode_lists(T,T,_,MediaInfo) -> gen_metadata(MediaInfo);
loop_decode_lists(Track,MaxTrack,<<?AVI_LIST,_ListSize:32/little,?AVI_SUB_LIST_FLAG,?AVI_SUBL_LIST_FLAG_H,StrHCD:32/little,StrHBin:StrHCD/binary,
								   ?AVI_SUBL_LIST_FLAG_F,StrFCD:32/little,StrFBin:StrFCD/binary,R/binary>>,#media_info{audio=Audios,video=Videos} = MediaInfo) ->
	case StrHBin of
		<<?AVI_FCC_TYPE_VIDS,_FccHandler:4/binary,_:6/binary,Language:16/little,_:4/binary,_DwScale:32/little,DwRate:32/little,_/binary>> ->
			<<_:4/binary,BiWidth:32/little,BiHeight:32/little,_:4/binary,BiCompression:4/binary,_/binary>> = StrFBin,
%% 			BitRate = BiHeight * BiWidth * DwRate,
			LowAtomFccHandler = list_to_atom(string:to_lower(binary_to_list(BiCompression))),
			NewDwScale = case proplists:get_value(LowAtomFccHandler,?TIMECLOCK_MAPPING) of
								undefined -> ?DEFAULT_H264_TIMECLOCK;
							 	V -> V
						 end,
			VideoStreamInfo = #stream_info{content=?FRAME_CONTENT_VIDEO,codec=LowAtomFccHandler,language=Language,timescale=NewDwScale,stream_id=Track,
										   params=#video_params{fps=DwRate,height=BiHeight,width=BiWidth}},
			loop_decode_lists(Track + 1,MaxTrack ,R,MediaInfo#media_info{video=[VideoStreamInfo|Videos]});
		<<?AVI_FCC_TYPE_AUDS,FccHandler:32/little,_:6/binary,Language:16/little,_:4/binary,DwScale:32/little,DwRate:32/little,_/binary>> ->
%% 			<<_:2/binary,NChannels:16/little,_:10/binary,DwRate1:16/little,_/binary>> = StrFBin,
%% 			AudioStreamInfo = #stream_info{content=?FRAME_CONTENT_AUDIO,codec=FccHandler,language=Language,timescale=DwScale,stream_id=Track,
%% 										   params=#audio_params{sample_rate=DwRate1,channels=NChannels}},
%% 			loop_decode_lists(Track + 1,MaxTrack ,R,MediaInfo#media_info{audio=[AudioStreamInfo|Audios]})
			loop_decode_lists(Track + 1,MaxTrack ,R,MediaInfo)
	end.
	
gen_metadata(#media_info{audio=Audios,video=Videos,metadata=Metadata} = MediaInfo) ->
	case lists:reverse(Videos) of
		[] ->
			?INFO("~p -- no video info.",[?MODULE]),
			{error,no_video_info};
		[#stream_info{codec=Fcc,stream_id=0,timescale=Timescale,params=Params,language=Language} | _T] = NewVideos ->
			NewMetadata = Metadata#stream_info{codec=Fcc,stream_id=0,timescale=Timescale,params=Params,language=Language},
			{ok,MediaInfo#media_info{audio=lists:reverse(Audios),video=NewVideos,metadata=NewMetadata}}
	end.
	
-spec decode_index(FD::any(),MediaInfo::#media_info{}) -> 
		  {ok,MediaInfo::#media_info{},IVIndexs::[],DataStartPosition::integer(),DataSize::integer()}|{error,Reason::any()}.
decode_index(FD,MediaInfo) ->
	case prim_file:read(FD, 12) of
		{ok,<<?AVI_DATA_LIST,DataSize:32/little,?AVI_DATA_LIST_FLAG>>} ->
			{ok,DataPosition} = prim_file:position(FD, {cur,0}),
			RelDataSize = DataSize-4,
			{ok,_} = prim_file:position(FD, DataPosition + RelDataSize),
			{ok,IVIndexs} = loop_decode_indexs(FD),
			{ok,MediaInfo,IVIndexs,DataPosition-4,RelDataSize};
		{ok,<<?AVI_LIST_JUNK,DataSize:32/little,_/binary>>} ->
			{ok,CurPos} = prim_file:position(FD, {cur,0}),
			{ok,_} = prim_file:position(FD, CurPos + DataSize - 4),
			decode_index(FD,MediaInfo);
		{error,Reason} ->
			?INFO("~p -- read avi index list error:~p",[?MODULE,Reason]),
			{error,decode_failed}
	end.

loop_decode_indexs(FD) ->
	case prim_file:read(FD, 8) of
		%% no indexs
		eof -> 
			?INFO("~p -- no indexs.",[?MODULE]),
			{ok,[]};
		{ok,<<?AVI_INDEX_LIST,IndexSize:32/little>>} ->
			{ok,Bin} = prim_file:read(FD, IndexSize),
			loop_decode_indexs0(Bin,[],[],[],0,0)
	end.

loop_decode_indexs0(<<>>,IVS,_VS,_AS,_,_) -> {ok,reverse_all(IVS)};
%% ignore pc index ?
loop_decode_indexs0(<<_:2/binary,?AVI_INDEX_FRAME_TYPE_PC,_:8/binary,R/binary>>,IVS,VS,AS,Account,AAcount) ->
	loop_decode_indexs0(R,IVS,VS,AS,Account,AAcount);
%% decode audio index.
loop_decode_indexs0(<<_:2/binary,?AVI_INDEX_FRAME_TYPE_AUDIO,_:4/binary,_Offset:32/little,_Size:32/little,R/binary>>,IVS,VS,AS,Account,AAcount) ->
%% 	loop_decode_indexs0(R,IVS,VS,[{AAcount,Offset,Size}|AS],Account,AAcount+1);
	loop_decode_indexs0(R,IVS,VS,AS,Account,AAcount+1);
%% decode video index.
%% loop_decode_indexs0(<<TrackBin:2/binary,VideoType:2/binary,_IFlag:4/binary,Offset:32/little,Size:32/little,R/binary>>,IVS,VS,AS,0,AAcount) ->
%% 	Track = list_to_integer(binary_to_list(TrackBin)),
%% 	case find_track(Track, IVS,[]) of
%% 		undefined ->
%% 			NewTrackInfo = #track_info{id=Track,type=VideoType,iindexs=[{0,Offset,Size}]},
%% 			loop_decode_indexs0(R,[NewTrackInfo|IVS],VS,AS,1,AAcount);
%% 		{#track_info{iindexs=SIVS} = TInfo,RestIVS} ->
%% 			NewTrackInfo = TInfo#track_info{iindexs=[{0,Offset,Size}|SIVS]},
%% 			loop_decode_indexs0(R,[NewTrackInfo|RestIVS],VS,AS,1,AAcount)
%% 	end;
loop_decode_indexs0(<<TrackBin:2/binary,VideoType:2/binary,IFlag:32/little,Offset:32/little,Size:32/little,R/binary>>,IVS,VS,AS,Account,AAcount) ->
	Track = list_to_integer(binary_to_list(TrackBin)),
	case IFlag of
		?AVI_INDEX_TYPE_I ->
			%% i index
			case find_track(Track, IVS,[]) of
				undefined ->
					NewTrackInfo = #track_info{id=Track,type=VideoType,iindexs=[{Account,Offset,Size}]},
					loop_decode_indexs0(R,[NewTrackInfo|IVS],VS,AS,Account+1,AAcount);
				{#track_info{iindexs=SIVS} = TInfo,RestIVS} ->
					NewTrackInfo = TInfo#track_info{iindexs=[{Account,Offset,Size}|SIVS]},
					loop_decode_indexs0(R,[NewTrackInfo|RestIVS],VS,AS,Account+1,AAcount)
			end;
		_ ->
			%% other index
			loop_decode_indexs0(R,IVS,VS,AS,Account+1,AAcount)
	end.

analyse_index_offset_type(IVS,StartOffset) ->
	analyse_index_offset_type0(IVS,{StartOffset,?AVI_INDEX_OFFSET_TYPE_ABSOLUTE}).

analyse_index_offset_type0([],{_,Type}) -> Type;
analyse_index_offset_type0([#track_info{iindexs=[]}|T],Type) ->
	analyse_index_offset_type0(T, Type);
analyse_index_offset_type0([#track_info{iindexs=[{_,Offset,_}|_]}|_],{St,_}) when Offset < St->
	?AVI_INDEX_OFFSET_TYPE_RELATIVE;
analyse_index_offset_type0([_|T],Type) ->
	analyse_index_offset_type0(T, Type).


%% ignore when is pc data.
loop_analyse_frames(From,ReadId,ReadLength,Frames,<<_:2/binary,?AVI_INDEX_FRAME_TYPE_PC,DataSize:32/little,_:DataSize/binary,R/binary>>,Info) 
  																when (DataSize rem 2) == 0 ->
	loop_analyse_frames(From, ReadId, ReadLength, Frames, R,Info);
loop_analyse_frames(From,ReadId,ReadLength,Frames,<<_:2/binary,?AVI_INDEX_FRAME_TYPE_PC,DataSize:32/little,_:DataSize/binary,_:1/binary,R/binary>>,Info) ->
	loop_analyse_frames(From, ReadId, ReadLength, Frames, R,Info);

%% ignore when is audio data.
loop_analyse_frames(From,ReadId,ReadLength,Frames,<<_:2/binary,?AVI_INDEX_FRAME_TYPE_AUDIO,DataSize:32/little,_:DataSize/binary,R/binary>>,Info) 
  																when (DataSize rem 2) == 0 ->
	loop_analyse_frames(From, ReadId, ReadLength, Frames, R,Info);
loop_analyse_frames(From,ReadId,ReadLength,Frames,<<_:2/binary,?AVI_INDEX_FRAME_TYPE_AUDIO,DataSize:32/little,_:DataSize/binary,_:1/binary,R/binary>>,Info)->
	loop_analyse_frames(From, ReadId, ReadLength, Frames, R,Info);

loop_analyse_frames(From,ReadId,_NextReadLength,Frames,<<?AVI_INDEX_LIST,_/binary>>,ReadInfo) ->
	callback_to(From, ReadId, Frames),
	?INFO("~p -- analyse frame eof",[?MODULE]),
	{eof,ReadInfo};
loop_analyse_frames(From,ReadId,ReadLength,Frames,<<TrackBin:2/binary,Flag:2/binary,R/binary>> = RestBin,
					#avi_read_header{vids_account=Account,fps=FPS,vids_fcc_handler=FccHandler}=ReadInfo) ->
	case lists:member(Flag, ?AVI_VIDEO_BINARY_FLAGS) of
		true ->
			case R of 
				<<DataSize:32/little,DataBin/binary>> when byte_size(DataBin) < DataSize ->
					callback_to(From, ReadId, Frames),
					{ok,ReadLength,ReadInfo,RestBin};
				<<DataSize:32/little,DataBin:DataSize/binary,RemainBin/binary>> when (DataSize rem 2) == 0->
					%% analyse frames
					Track = list_to_integer(binary_to_list(TrackBin)),
					Timestamp = Account * FPS,
					Frame = #frame{data=DataBin,type=FccHandler,track=Track,timestamp=Timestamp,duration=Timestamp*90,timescale = 90000},
%% 					?TRACK("div 2 0,frame:~p size:~p",[{Account,Timestamp},DataSize]),
					loop_analyse_frames(From, ReadId, ReadLength - 1, [Frame|Frames], RemainBin,ReadInfo#avi_read_header{vids_account=Account+1});
				<<DataSize:32/little,DataBin:DataSize/binary,_IgnoreBin:1/binary,RemainBin/binary>> ->
					Track = list_to_integer(binary_to_list(TrackBin)),
					Timestamp = Account * FPS,
					Frame = #frame{data=DataBin,type=FccHandler,track=Track,timestamp=Timestamp,duration=Timestamp*90,timescale = 90000},
%% 					?TRACK("frame:~p size:~p",[{Account,Timestamp},DataSize]),
					loop_analyse_frames(From, ReadId, ReadLength - 1, [Frame|Frames], RemainBin,ReadInfo#avi_read_header{vids_account=Account+1});
				_ ->
					callback_to(From, ReadId, Frames),
					{ok,ReadLength,ReadInfo,RestBin}
			end;
		false ->
			%% some error in source data ignore 4 byte.
			loop_analyse_frames(From,ReadId,ReadLength,Frames,R,ReadInfo)
			
	end;
loop_analyse_frames(From,ReadId,NextReadLength,Frames,RestBin,ReadInfo) ->
	callback_to(From, ReadId, Frames),
	{ok,NextReadLength,ReadInfo,RestBin}.


find_track(_, [],_) -> undefined;
find_track(Track,[#track_info{id=Track}=TInfo|T],Rest) ->
	{TInfo,lists:merge(T, Rest)};
find_track(Track, [Info|T], Rest) ->
	find_track(Track, T, [Info|Rest]).

search_track(_Track, []) -> undefined;
search_track(Track, [#track_info{id=Track}=Info|_T]) ->
	Info;
search_track(Track,[_|T]) ->
	search_track(Track, T).
	

reverse_all(IVS) ->
	reverse_all0(IVS,[]).

reverse_all0([],IVS) -> IVS;
reverse_all0([#track_info{iindexs=SIVS}=Info|T],IVS) ->
	reverse_all0(T,[Info#track_info{iindexs=lists:reverse(SIVS)}|IVS]).


search_iindexs(_StartPos,_FPS,{PreAccount,PreOffset},[]) -> {PreAccount,PreOffset};
search_iindexs(StartPos,FPS,{PreAccount,PreOffset},[{IIndex,_,_}|_]) when IIndex*FPS > StartPos->
	{PreAccount,PreOffset};
search_iindexs(StartPos,FPS,{PreAccount,PreOffset},[{IIndex,Offset,_}|T]) ->
	search_iindexs(StartPos,FPS,{IIndex,Offset},T).

	
count_new_offset(_StartOffset,Offset,?AVI_INDEX_OFFSET_TYPE_ABSOLUTE) -> Offset;
count_new_offset(StartOffset,Offset,?AVI_INDEX_OFFSET_TYPE_RELATIVE) ->
	StartOffset + Offset.

read_first_frame(FD,StartOffset,#media_info{video=Videos} = MediaInfo) ->
	{ok,_} = prim_file:position(FD, StartOffset),
	case prim_file:read(FD, 8) of
		{ok,<<TrackBin:2/binary,_Flag:2/binary,DataSize:32/little>>} ->
			Track = list_to_integer(binary_to_list(TrackBin)),
			case prim_file:read(FD, DataSize) of
				{ok,FrameBin} ->
					{ok,NewVideos} = analyse_config(Track,Videos,FrameBin,[]),
%% 					?TRACK("new videos:~p",[NewVideos]),
					{ok,MediaInfo#media_info{video=NewVideos}};
				_ ->
					{ok,MediaInfo}
			end;
		E ->
			?INFO("~p -- read first frame error:~p",[?MODULE,E]),
			{ok,MediaInfo}
	end.

analyse_config(_Track,[],_FrameBin,L) -> {ok,lists:reverse(L)};
analyse_config(Track,[#stream_info{stream_id=Track,codec=Codec,options=Opts}=Info|T],FrameBin,L) ->
	Options = unpacket_sps_pps_frame(Codec,FrameBin),
	analyse_config(Track,T,FrameBin,[Info#stream_info{options=lists:append(Opts, Options)}|L]);
	
analyse_config(Track,[Info|T],FrameBin,L) ->
	analyse_config(Track,T,FrameBin,[Info|L]).

unpacket_sps_pps_frame(h264,FrameBin) -> unpacket_sps_pps0(FrameBin,[]);
unpacket_sps_pps_frame(_,_) -> [].

unpacket_sps_pps0(<<0,0,0,1,R/binary>>,Options) ->
	unpacket_next_sps_pps(R,Options);
unpacket_sps_pps0(<<0,0,1,R/binary>>,Options) ->
	unpacket_next_sps_pps(R,Options).
%% pps
unpacket_next_sps_pps(<<0:1, _NalRefIdc:2, ?NAL_PPS:5, R/binary>> = Frame,Options) ->
	PPSSize = loop_read_rest_size(R,0) +1,
  	<<PPS:PPSSize/binary,Rest/binary>> = Frame,
  	unpacket_sps_pps0(Rest, [{pps,PPS}|Options]);
%% sps
unpacket_next_sps_pps(<<0:1, _NalRefIdc:2, ?NAL_SPS:5, R/binary>> = Frame,Options) ->
	SPSSize = loop_read_rest_size(R,0) + 1,
  	<<SPSBin:SPSSize/binary,Rest/binary>> = Frame,
  	unpacket_sps_pps0(Rest, [{sps,SPSBin}|Options]);
unpacket_next_sps_pps(_,Options) ->
	Options.

loop_read_rest_size(<<0,0,0,1,_/binary>>,Size) -> Size;
loop_read_rest_size(<<0,0,1,_/binary>>,Size) -> Size;
loop_read_rest_size(<<_,0,0,R/binary>>,Size) ->
	loop_read_rest_size(<<0,0,R/binary>>,Size+1);
loop_read_rest_size(<<_,_,0,R/binary>>,Size) ->	
	loop_read_rest_size(<<0,R/binary>>,Size+2);
 loop_read_rest_size(<<_,_,_,R/binary>>,Size) ->
	loop_read_rest_size(R,Size+3).
	 
	 