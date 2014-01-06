%% Author: sunshine
%% Created: 2014-1-2
%% Description: TODO: Add description to mp4_util
-module(mp4_util).

%%
%% Include files
%%
-include("log.hrl").
-include("h264.hrl").
-include("aac.hrl").
-include("media_info.hrl").
-include("rtsp.hrl").
-include("rtp_session.hrl").
-include("video_file.hrl").
-include("video_mpg4.hrl").
-include_lib("kernel/include/file.hrl").
%%
%% Exported Functions
%%
-export([analyze_media_info/1,position/2,decode_mpg4_file/2,read_frames/3]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

decode_mpg4_file(FilePath,FD) ->
	{ok,#file_info{size=TotalFileSize}} = prim_file:read_file_info(FilePath),
	analyse_blocks(FD,TotalFileSize,#mp4_media{}).

analyse_blocks(FD,TotalFileSize,#mp4_media{} = MediaInfo) ->
	case read_block_header(FD, TotalFileSize) of
		{ok,Type,RestSize} -> 
			{ok,MediaInfo1} = box(Type,FD,RestSize,MediaInfo),
			analyse_blocks(FD, TotalFileSize, MediaInfo1);
		eof ->
			{ok,MediaInfo};
		_ ->
			?INFO("~p -- read ftyp info error:~p",[?MODULE,invalide_flag]),
			{error,decode_failed}
	end. 

box(?BOX_FTYP,FD,Size,MediaInfo) ->
	case prim_file:read(FD, Size) of
		{ok,FtypBin} ->
			{ok,FtypBox} = mp4:box_ftyp(FtypBin),
			{ok,MediaInfo#mp4_media{ftyp=FtypBox}};
		{error,_E} ->
			{error,decode_failed}
	end;
box(?BOX_MOOV,FD,Size,MediaInfo) ->
 	{ok,MoovBin} = prim_file:read(FD, Size),
	{ok,MoovBox} = mp4:moov_box(MoovBin),
	{ok,MediaInfo#mp4_media{moov=MoovBox}};
box(Type,FD,Size,MediaInfo) ->
	?INFO("~p -- ignore box:~p",[?MODULE,Type]),
	{ok,_} = prim_file:position(FD, {cur,Size}),
	{ok,MediaInfo}.



read_block_header(FD,TotalFileSize) ->
	case prim_file:read(FD, 8) of
		{ok,<<1:32,Type:4/binary>>} ->
			{ok,<<LargeSize:64>>} = prim_file:read(FD, 8),
			{ok,binary_to_list(Type),LargeSize - 16};
		{ok,<<0:32,Type:4/binary>>} ->
			{ok,Pos} = prim_file:position(FD, {cur,0}),
			{ok,binary_to_list(Type),TotalFileSize - Pos};
		{ok,<<Size:32,?BOX_UUID>>} ->
			{ok,ExtType} = prim_file:read(FD, 16),
			{ok,binary_to_list(ExtType),Size - 24};
		{ok,<<Size:32,Type:4/binary>>} ->
			{ok,binary_to_list(Type),Size - 8};
		eof -> eof
	end.

position(#mpg4_index{samples=[]},_Pos) ->
	?INFO("~p -- no trunk to position",[?MODULE]),
	{error,not_found};
position(#mpg4_index{specific=Spec,stss=Stss,samples=Samples,timescale=Timescale},Pos) ->
	PosDuration = Pos * Timescale div 1000,
	SampleIndex = search_sample(PosDuration,Samples,0,1),
	SampleIIndex = case Stss of
						 undefined -> SampleIndex;
						 _ -> 
							 search_iindex_sample(SampleIndex,Stss,undefined)
				  	 end,
	{ok,Reader} = gen_trunk_reader(SampleIIndex,Samples,#mpg4_reader{timescale=Timescale,specific=Spec}),
%% 	?TRACK("pos:~p posdur:~p sampleindex:~p iindex:~p",[Pos,PosDuration,SampleIndex,SampleIIndex]),
	{ok,Reader}.

search_iindex_sample(SampleIndex,[],undefined) -> SampleIndex;
search_iindex_sample(_,[],PreIIndex) -> PreIIndex;
search_iindex_sample(SampleIndex,[IIndex|_T],undefined) when SampleIndex =< IIndex -> IIndex;
search_iindex_sample(SampleIndex,[IIndex|_T],PreIIndex) when SampleIndex < IIndex -> PreIIndex;
search_iindex_sample(SampleIndex,[IIndex|T],_PreIIndex) ->
	search_iindex_sample(SampleIndex,T,IIndex).

search_sample(_,[],_,PreIndex) -> PreIndex;
search_sample(PosDuration,[{trunk,_}|T],TotalDuration,PreIndex) ->
	search_sample(PosDuration,T,TotalDuration,PreIndex);
search_sample(PosDuration,[{Index,_,Duration}|T],TotalDuration,PreIndex) ->
	case (TotalDuration+Duration) > PosDuration of
		true -> PreIndex;
		false ->
			search_sample(PosDuration,T,TotalDuration+Duration,Index)
	end.

gen_trunk_reader(Index,[{trunk,Offset}|T],Reader) ->
	gen_trunk_reader(Index, T, Reader#mpg4_reader{cur_offset=Offset});
gen_trunk_reader(Index,[{CurIndex,_,_}|_]=Samples,Reader) when CurIndex >= Index-> 
	{ok,Reader#mpg4_reader{samples=Samples}};
gen_trunk_reader(Index,[{_CurIndex,Offset,Duration}|T],#mpg4_reader{cur_duration=CurDuration,cur_offset=CurOffset}=Reader) ->
	gen_trunk_reader(Index, T, Reader#mpg4_reader{cur_duration=CurDuration+Duration,cur_offset=CurOffset+Offset}).

analyze_media_info(#mp4_media{moov=#box_moov{traks=Tracks,mvhd=#box_moov_mvhd{duration=Duration,timescale=Timescale}}}) ->
	MediaInfo = #media_info{flow_type=?FLOW_TYPE_FILE,duration=(Duration*1000 /Timescale)},
	analyze_tracks(MediaInfo,Tracks,[]).

analyze_tracks(#media_info{video=Videos,audio=Audios}=MediaInfo,[],Readers) -> 
	{ok,MediaInfo#media_info{video=lists:reverse(Videos),audio=lists:reverse(Audios)},lists:sort(Readers)};
analyze_tracks(#media_info{video=Videos}=MediaInfo,[#box_moov_trak{mdia=#box_moov_trak_mdia{minf=#box_moov_trak_mdia_minf{stbl=#box_moov_trak_mdia_minf_stbl{
																		    stsz=Stsz,stz2=Stsz2,stts=#box_moov_trak_mdia_minf_stbl_stts{tts=Stts},
																			stsc=#box_moov_trak_mdia_minf_stbl_stsc{entries=Stsc},stco=Stco,co64=Co64,
																			stss=#box_moov_trak_mdia_minf_stbl_stss{entries=Stss},
																			stsd=#box_moov_trak_mdia_minf_stbl_stsd{entries=[SampleDesBox|_]}}
																														 },
																  mdhd=#box_moov_trak_mdia_mdhd{duration=Duration,timescale=Timescale,language=Language},
																  hdlr=#box_moov_trak_mdia_hdlr{handler_type=?TRAK_TYPE_VIDEO}},
										 tkhd=#box_moov_trak_tkhd{track_id=TrackId}}|T],Readers) ->    
	SampleCount = case Stsz of
					#box_moov_trak_mdia_minf_stbl_stsz{sample_count=SampleCount0} ->
						SampleCount0;
					undefined ->
						#box_moov_trak_mdia_minf_stbl_stz2{sample_count=SampleCount0} = Stsz2,
						SampleCount0
				 end,
	#stsd_video_sample_entry{height=Height,width=Width,specific=H264} = SampleDesBox,
	Fps = round(SampleCount*100 / (Duration / Timescale))/100,
	Videoparams = #video_params{height=Height,width=Width,fps=Fps},
	StreamInfo = #stream_info{codec=h264,stream_id=TrackId,language=Language,content=?FRAME_CONTENT_VIDEO,timescale=Timescale,
							  options=[{h264,H264}],params=Videoparams,mic_sec_per_frame=Fps},
	IndexStsz = case Stsz of
					undefined ->  
						#box_moov_trak_mdia_minf_stbl_stz2{tz2=Sizes} = Stsz2,
						Sizes;
					#box_moov_trak_mdia_minf_stbl_stsz{sample_size=0,tsz=Sizes} -> 
						Sizes;
					#box_moov_trak_mdia_minf_stbl_stsz{sample_size=SimpleSize} ->
						{all,SimpleSize}
				end,
	{TrunkSize,IndexStco} = case Stco of
				 undefined -> 
					 #box_moov_trak_mdia_minf_stbl_co64{entry_count=TrunkSize,entries=Entries} = Co64,
					 {TrunkSize,Entries};
				 #box_moov_trak_mdia_minf_stbl_stco{entry_count=TrunkSize,entries=Entries} -> 
					 {TrunkSize,Entries}
				end,
	{ok,AllSamples} = gen_samples(TrunkSize,Stsc,IndexStco,Stts,IndexStsz,[],1),
	Reader = #mpg4_index{timescale=Timescale,samples=AllSamples,stss=Stss,specific=H264},
	analyze_tracks(MediaInfo#media_info{video=[StreamInfo|Videos]},T,[{TrackId,Reader}|Readers]);
%% analyze_tracks(#media_info{audio=Audios}=MediaInfo,[#box_moov_trak{mdia=#box_moov_trak_mdia{minf=#box_moov_trak_mdia_minf{stbl=#box_moov_trak_mdia_minf_stbl{
%% 																		    stsz=Stsz,stz2=Stsz2,
%% 																			stsd=#box_moov_trak_mdia_minf_stbl_stsd{entries=[SampleDesBox|_]}}=Stbl
%% 																														 },
%% 																  mdhd=#box_moov_trak_mdia_mdhd{duration=Duration,timescale=Timescale,language=Language},
%% 																  hdlr=#box_moov_trak_mdia_hdlr{handler_type=?TRAK_TYPE_AUDIO}},
%% 										 tkhd=#box_moov_trak_tkhd{track_id=TrackId}}|T],Stbls) ->    
%% 	SampleCount = case Stsz of
%% 					#box_moov_trak_mdia_minf_stbl_stsz{sample_count=SampleCount0} ->
%% 						SampleCount0;
%% 					undefined ->
%% 						#box_moov_trak_mdia_minf_stbl_stz2{sample_count=SampleCount0} = Stsz2,
%% 						SampleCount0
%% 				 end,
%% 	case SampleDesBox of
%% 		#stsd_audio_sample_entry{esds=#esds{avg_bitrate=BitRate,object_type='aac',
%% 					specific=#aac_config{channel_count=ChannelCount,sample_rate=SampleRate}=Config}} ->
%% 			Audioparams = #audio_params{channels=ChannelCount,sample_rate=SampleRate},
%% 			Fps = round(SampleCount*100 / (Duration / Timescale))/100,
%% 			StreamInfo = #stream_info{codec=aac,stream_id=TrackId,language=Language,content=?FRAME_CONTENT_AUDIO,timescale=Timescale,
%% 							  options=[{aac,Config}],params=Audioparams,bitrate=BitRate,mic_sec_per_frame=Fps},
%% 			analyze_tracks(MediaInfo#media_info{audio=[StreamInfo|Audios]},T,[{TrackId,Stbl}])
%% 	end;
analyze_tracks(MediaInfo,[_|T],L) ->
	analyze_tracks(MediaInfo, T,L).



gen_samples(0,_,_IndexStco,_Stts,_IndexStsz,L,_index) -> {ok,lists:reverse(L)};
gen_samples(TrunkSize,[{TrunkIndex,SampleSize,Any}],[TrunkOffset|TrunkOffsets],TimesToSamples,SamplesSize,L,Index) ->
	{Samples,NewTimesToSamples,NewSamplesSize} = gen_trunk_samples(SampleSize,TimesToSamples,SamplesSize,[{trunk,TrunkOffset}|L],Index),
	gen_samples(TrunkSize - 1, [{TrunkIndex+1,SampleSize,Any}], TrunkOffsets, NewTimesToSamples, NewSamplesSize, Samples,Index+SampleSize);

gen_samples(TrunkSize,[{TrunkIndex,SampleSize,Any},{NextTrunkIndex,NextSampleSize,NAny}|T],[TrunkOffset|TrunkOffsets],
			TimesToSamples,SamplesSize,L,Index) ->
	{Samples,NewTimesToSamples,NewSamplesSize} = gen_trunk_samples(SampleSize,TimesToSamples,SamplesSize,[{trunk,TrunkOffset}|L],Index),
	case (TrunkIndex+1) of
		NextTrunkIndex ->
			gen_samples(TrunkSize - 1, [{NextTrunkIndex,NextSampleSize,NAny}|T], TrunkOffsets, NewTimesToSamples, NewSamplesSize, Samples,Index+SampleSize);
		_ ->
			gen_samples(TrunkSize - 1, [{TrunkIndex+1,SampleSize,Any},{NextTrunkIndex,NextSampleSize,NAny}|T], 
						TrunkOffsets, NewTimesToSamples, NewSamplesSize, Samples,Index+SampleSize)
	end.
gen_trunk_samples(0,TimesToSamples,SamplesSize,L,_index) -> {L,TimesToSamples,SamplesSize};
gen_trunk_samples(SampleSize,[{0,_Duration}|T],SamplesSize,L,Index) ->
	gen_trunk_samples(SampleSize,T,SamplesSize,L,Index);
gen_trunk_samples(SampleSize,[{Count,Duration}|T],{all,Size} = SamplesSize,L,Index) ->
	SampleInfo = {Index,Size,Duration},
	gen_trunk_samples(SampleSize - 1,[{Count-1,Duration}|T],SamplesSize,[SampleInfo|L],Index+1);
gen_trunk_samples(SampleSize,[{Count,Duration}|T],[Size|SamplesSize],L,Index) ->
	SampleInfo = {Index,Size,Duration},
	gen_trunk_samples(SampleSize - 1,[{Count-1,Duration}|T],SamplesSize,[SampleInfo|L],Index+1).


read_frames(_,#mpg4_reader{fd=FD,samples=[]},Frames) -> 
	prim_file:close(FD),
	{eof,[eof|Frames]};
read_frames(RestReadLength,Reader,Frames) when RestReadLength =< 0 -> {ok,Reader,Frames};
read_frames(RealReadLength,#mpg4_reader{samples=[{trunk,Offset}|Samples],cur_offset=Offset} = Reader,Frames) ->
	read_frames(RealReadLength, Reader#mpg4_reader{samples=Samples},Frames);
read_frames(RealReadLength,#mpg4_reader{fd=FD,samples=[{trunk,Offset}|Samples]} = Reader,Frames) ->
	{ok,_} = prim_file:position(FD, Offset),
	read_frames(RealReadLength, Reader#mpg4_reader{samples=Samples,cur_offset=Offset},Frames);
read_frames(RealReadLength, #mpg4_reader{fd=FD,samples=Samples,timescale=TimeScale,cur_offset=Offset,track=Track,type=Type,cur_duration=Duration} = Reader,Frames) ->
	{ok,RestReadLength,ReadSamples,RestSamples,TotalReadSize,TotalDuration} = amount_samples(Samples,RealReadLength,[],0,0),
	case prim_file:read(FD, TotalReadSize) of
		eof -> 
			prim_file:close(FD),
			{eof,[eof|Frames]};
		{ok,Data} ->
			{ok,NewFrames} = anaylse_frames(Data,ReadSamples,TimeScale,Duration,Track,Type,Frames),
			read_frames(RestReadLength, Reader#mpg4_reader{samples=RestSamples,cur_offset=Offset+TotalReadSize,
														   cur_duration=Duration+TotalDuration},NewFrames);
		{error,R} -> 
			catch prim_file:close(FD),
			{error,R}
	end.
	
amount_samples([],RestLength,L,Size,Dur) -> {ok,RestLength,lists:reverse(L),[],Size,Dur};
amount_samples([{trunk,_}|_]=Samples, RestLength, L,Size,Dur) -> {ok,RestLength,lists:reverse(L),Samples,Size,Dur};
amount_samples([{_,SSize,SDur} =Sample|T], RestLength, L,Size,Dur) ->
	amount_samples(T, RestLength-1, [Sample|L],Size+SSize,Dur+SDur).

anaylse_frames(<<>>,_ReadSamples,_TimeScale,_Duration,_Track,_Type,L) -> {ok,L};
anaylse_frames(_,[],_TimeScale,_Duration,_Track,_Type,L) -> {ok,L};
anaylse_frames(R,[{_Index,Size,Dur}|T],TimeScale,Duration,Track,Type,L) ->
%% 	DataSize = Size -4,
	<<Bin:Size/binary,Rest/binary>> = R,
	Timestamp = Duration * 1000 div TimeScale,
	FrameBin = anaylse_frame_bin(Bin),
	case FrameBin of
		<<1,_/binary>> ->
			anaylse_frames(Rest, T, TimeScale, Duration+Dur,Track,Type,L);
		_ ->
			Frame = #frame{timescale=TimeScale,duration=Duration,data=FrameBin,timestamp=Timestamp,track=Track,type=Type},
%% 	Frame = anaylse_frames0(Bin,TimeScale,Duration,Track,Type),
			anaylse_frames(Rest, T, TimeScale, Duration+Dur,Track,Type,[Frame|L])
	end.
	

anaylse_frame_bin(<<Size:32,Body:Size/binary>> = Bin) -> Body;
anaylse_frame_bin(<<Size:32,B:Size/binary,R/binary>>) ->
	anaylse_frame_bin0(R,B).

anaylse_frame_bin0(<<>>,B) -> B;
anaylse_frame_bin0(<<Size:32,B:Size/binary,R/binary>>,Body) ->
	anaylse_frame_bin0(R, <<Body/binary,B/binary>>).

