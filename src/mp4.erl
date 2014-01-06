%% Author: sunshine
%% Created: 2013-12-27
%% Description: TODO: Add description to mp4
-module(mp4).

%%
%% Include files
%%
-include("log.hrl").
-include("video_mpg4.hrl").
%%
%% Exported Functions
%%
-export([box_ftyp/1,moov_box/1,moov_box_track/1,moov_box_track_mdia/1,moov_box_track_mida_minf/1,
		 moov_box_track_mida_minf_stbl/1,audio_esds/1]).
%%
%% API Functions
%%

box_ftyp(FtypBin) ->
	box_ftyp(FtypBin,#box_ftyp{}).
	
moov_box(MoovBin) ->
	moov_box(MoovBin,#box_moov{}).

moov_box_track(TrackBin) ->
	moov_box_track(TrackBin,#box_moov_trak{}).

moov_box_track_mdia(Body) ->
	moov_box_track_mdia(Body,#box_moov_trak_mdia{}).

moov_box_track_mida_minf(Body) ->
	moov_box_track_mida_minf(Body,#box_moov_trak_mdia_minf{}).

moov_box_track_mida_minf_stbl(Body) ->
  moov_box_track_mida_minf_stbl(Body,#box_moov_trak_mdia_minf_stbl{}).
	
%%
%% Local Functions
%%
box_ftyp(<<MajorBrand:4/binary,MinorVersion:32,Brands/binary>>,#box_ftyp{}=Ftyp) ->
	{ok,CompatibleBrands} = loop_analyse_brands(Brands,[]),
	{ok,Ftyp#box_ftyp{major_brand=MajorBrand,minor_version=MinorVersion,compatible_brands=CompatibleBrands}}.


moov_box(R,#box_moov{traks=Tracks} = Moov) ->
	case read_box_header(R) of
		%% moov.mvhd box. and version = 0.
		{?BOX_MOOV_MVHD,<<0,_:3/binary,CreationTime:32,ModifyTime:32,Timescale:32,Duration:32,
		   				Rate:32,Volume:16,_:2/binary,_:8/binary,Matrix:36/binary,
						  PreDefined:24/binary,NextTrackId:32>>,Rest} ->
			moov_box(Rest, Moov#box_moov{version = 0,
							  mvhd=#box_moov_mvhd{creation_time=CreationTime,duration=Duration,matrix=Matrix,
												  next_track_id=NextTrackId,pre_defined=PreDefined,rate=Rate,
												  timescale=Timescale,version=0,volume=Volume,modify_time=ModifyTime
												  }});
		%% moov.mvhd box. and version = 1.
		{?BOX_MOOV_MVHD,<<1,_:3/binary,CreationTime:64,ModifyTime:64,Timescale:32,Duration:64,
		   					Rate:32,Volume:16,_:2/binary,_:8/binary,Matrix:36/binary,
						 	 PreDefined:24/binary,NextTrackId:32>>,Rest} ->
			moov_box(Rest, Moov#box_moov{version = 1,
							  mvhd=#box_moov_mvhd{creation_time=CreationTime,duration=Duration,matrix=Matrix,
												  next_track_id=NextTrackId,pre_defined=PreDefined,rate=Rate,
												  timescale=Timescale,version=1,volume=Volume,modify_time=ModifyTime
												  }});
		%% moov.mvhd box.
		{?BOX_MOOV_TRAK,Body,Rest} ->
			{ok,Track} = moov_box_track(Body),
			moov_box(Rest, Moov#box_moov{traks=lists:reverse([Track|Tracks])});
		{Type,Body,Rest} ->
			?INFO("~p -- ignore type:moov.~p size:~p",[?MODULE,Type,byte_size(Body)]),
			moov_box(Rest,Moov);
		eof ->
			{ok,Moov}
	end.

moov_box_track(R,#box_moov_trak{} = Trak) ->
	case read_box_header(R) of
		%% moov.box.track.tkhd version = 0
		{?BOX_MOOV_TRAK_TKHD,<<0,_Flag:3/binary,CreationTime:32,ModifyTime:32,TrackId:32,_:4/binary,Duration:32,
				 _:8/binary,Layer:16,AltGroup:16,Volume:16,_:2/binary,Matrix:36/binary,Width:32,Height:32>>,Rest} ->
			Tkhd = #box_moov_trak_tkhd{creation_time=CreationTime,duration=Duration,height=Height,layer=Layer,alter_group=AltGroup,
								matrix=Matrix,modify_time=ModifyTime,track_id=TrackId,volume=Volume,width=Width},
			moov_box_track(Rest, Trak#box_moov_trak{tkhd=Tkhd});
		%% moov.box.track.tkhd version = 1
		{?BOX_MOOV_TRAK_TKHD,<<1,_Flag:3/binary,CreationTime:64,ModifyTime:64,TrackId:32,_:4/binary,Duration:64,
				 _:8/binary,Layer:16,AltGroup:16,Volume:16,_:2/binary,Matrix:36/binary,Width:32,Height:32>>,Rest} ->
			Tkhd = #box_moov_trak_tkhd{creation_time=CreationTime,duration=Duration,height=Height,layer=Layer,alter_group=AltGroup,
								matrix=Matrix,modify_time=ModifyTime,track_id=TrackId,volume=Volume,width=Width},
			moov_box_track(Rest,Trak#box_moov_trak{tkhd=Tkhd});
		{?BOX_MOOV_TRAK_MDIA,Body,Rest} ->
			{ok,Mdia} = moov_box_track_mdia(Body),
			moov_box_track(Rest,Trak#box_moov_trak{mdia=Mdia});
		{Type,Body,Rest} ->
			?INFO("~p -- ignore type:moov.trak.~p size:~p",[?MODULE,Type,byte_size(Body)]),
			moov_box(Rest,Trak);
		eof ->
			{ok,Trak}
	end.

moov_box_track_mdia(R,#box_moov_trak_mdia{} = Mdia) ->
	case read_box_header(R) of
		%% moov.trak.mdia.mdhd version=0
		{?BOX_MOOV_TRAK_MDIA_MDHD,<<0,_:3/binary,CreationTime:32,ModifyTime:32,Timescale:32,Duration:32,
					  _:1,Language:15,_:2/binary>>,Rest} ->
			Mdhd = #box_moov_trak_mdia_mdhd{creation_time=CreationTime,modify_time=ModifyTime,duration=Duration,
									language=Language,timescale=Timescale,version=0},
			moov_box_track_mdia(Rest, Mdia#box_moov_trak_mdia{mdhd=Mdhd});
		%% moov.trak.mdia.mdhd version=1
		{?BOX_MOOV_TRAK_MDIA_MDHD,<<1,_:3/binary,CreationTime:64,ModifyTime:64,Timescale:32,Duration:64,
					  _:1,Language:15,_:2/binary>>,Rest} ->
			Mdhd = #box_moov_trak_mdia_mdhd{creation_time=CreationTime,modify_time=ModifyTime,duration=Duration,
									language=Language,timescale=Timescale,version=1},
			moov_box_track_mdia(Rest, Mdia#box_moov_trak_mdia{mdhd=Mdhd});
		{?BOX_MOOV_TRAK_MDIA_HDLR,<<Version:8,_:3/binary,_:4/binary,HandlerTypeBin:4/binary,_:12/binary,NameBin/binary>>,Rest} ->
			NameSize = byte_size(NameBin) - 1,
			<<Name:NameSize/binary,0>> = NameBin,
			HandlerType = ?BIN_TYPE_TO_ATOM(HandlerTypeBin),
			Hdlr = #box_moov_trak_mdia_hdlr{handler_type=HandlerType,name=binary_to_list(Name),version=Version},
			moov_box_track_mdia(Rest, Mdia#box_moov_trak_mdia{hdlr=Hdlr});
		{?BOX_MOOV_TRAK_MDIA_MINF,Body,Rest} ->
			{ok,Minf} = moov_box_track_mida_minf(Body),
			moov_box_track_mdia(Rest, Mdia#box_moov_trak_mdia{minf=Minf});
		{Type,Body,Rest} ->
			?INFO("~p -- ignore type:moov.trak.mdia.~p size:~p",[?MODULE,Type,byte_size(Body)]),
			moov_box_track_mdia(Rest,Mdia);
		eof ->
			{ok,Mdia}
	end.
moov_box_track_mida_minf(R,#box_moov_trak_mdia_minf{} = Minf) ->
	case read_box_header(R) of
		%% moov.trak.mida.minf.vmhd
		{'vmhd',<<Version,_:3/binary,Graphicsmod:16,Opc1:16,Opc2:16,Opc3:16>>,Rest}->
			Vmhd = #box_moov_trak_mdia_minf_vmhd{graphicsmode=Graphicsmod,opcolor={Opc1,Opc2,Opc3},version=Version},
			moov_box_track_mida_minf(Rest,Minf#box_moov_trak_mdia_minf{xmhd=Vmhd});
		%% moov.trak.mida.minf.smhd
		{'smhd',<<Version,_:3/binary,Balance:16,_:2/binary>>,Rest} ->
			Smhd = #box_moov_trak_mdia_minf_smhd{balance=Balance,version=Version},
			moov_box_track_mida_minf(Rest,Minf#box_moov_trak_mdia_minf{xmhd=Smhd});
		{'hmhd',<<Version,_:3/binary,MaxPDUsize:16,AvgPDUsize:16,Maxbitrate:32,Avgbitrate:32,_:4/binary>>,Rest} ->
			Hmhd = #box_moov_trak_mdia_minf_hmhd{version=Version,avgPDUsize=AvgPDUsize,avgbitrate=Avgbitrate,maxPDUsize=MaxPDUsize,maxbitrate=Maxbitrate},
			moov_box_track_mida_minf(Rest,Minf#box_moov_trak_mdia_minf{xmhd=Hmhd});
		{'nmhd',<<Version,_:3/binary,Flag:24>>,Rest} ->
			Nmhd = #box_moov_trak_mdia_minf_nmhd{version=Version,flag=Flag},
			moov_box_track_mida_minf(Rest,Minf#box_moov_trak_mdia_minf{xmhd=Nmhd});
		{'stbl',Body,Rest} ->
			{ok,Stbl} = moov_box_track_mida_minf_stbl(Body),
			moov_box_track_mida_minf(Rest, Minf#box_moov_trak_mdia_minf{stbl=Stbl});
		{Type,Body,Rest} ->
			?INFO("~p -- ignore type:moov.trak.mdia.minf.~p size:~p~n",[?MODULE,Type,byte_size(Body)]),
			moov_box_track_mida_minf(Rest,Minf);
		eof ->
			{ok,Minf}	
	end.

moov_box_track_mida_minf_stbl(R,#box_moov_trak_mdia_minf_stbl{} = Stbl) ->
	case read_box_header(R) of
		%% moov.trak.mida.minf.stbl.stts
		{'stts',<<Version,_:3/binary,EntyCount:32,SttsTableBin/binary>>,Rest} ->
			{ok,TTS} = loop_anaylse_tts(SttsTableBin,EntyCount,[]),
			moov_box_track_mida_minf_stbl(Rest,Stbl#box_moov_trak_mdia_minf_stbl{stts=
				#box_moov_trak_mdia_minf_stbl_stts{entiy_count=EntyCount,version=Version,tts=TTS}});
		%% moov.trak.mida.minf.stbl.stsd
		{'stsd',<<Version,_:3/binary,EntryCount:32,StsdBin/binary>>,Rest} ->
			{ok,Entries} = loop_stsd_analyse_entries(StsdBin,EntryCount,[]),
			Stsd = #box_moov_trak_mdia_minf_stbl_stsd{entries=Entries,version=Version},
			moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stsd=Stsd});
		%% moov.trak.mida.minf.stbl.stbl
		{'stsz',<<Version,_:3/binary,SampleSize:32,SampleCount:32,StszBin/binary>>,Rest} ->
			case SampleSize of
				0 ->
					case SampleCount of
						0 ->
							{ok,NewSampleCount,Tsz} = loop_analyse_tsz_without_count(StszBin,0,[]),
							Stsz = #box_moov_trak_mdia_minf_stbl_stsz{version=Version,sample_size=0,sample_count=NewSampleCount,tsz=Tsz},
							moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stsz=Stsz});
						_ ->
							{ok,Tsz} = loop_analyse_tsz(StszBin,SampleCount,[]),
%% 							?TRACK("count samplesize:~p size:~p~n",[SampleCount,count_size(Tsz,0)]),
							Stsz = #box_moov_trak_mdia_minf_stbl_stsz{version=Version,sample_size=0,sample_count=SampleCount,tsz=Tsz},
							moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stsz=Stsz})
					end;
				_ ->
					Stsz = #box_moov_trak_mdia_minf_stbl_stsz{version=Version,sample_size=SampleSize,sample_count=SampleCount},
					moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stsz=Stsz})
			end;
		{'stz2',<<Version,_:3/binary,_:3/binary,FieldSize:8,SampleCount:32,Stz2Bin/binary>>,Rest} ->
			{ok,Tz2} = loop_analyse_stz2(Stz2Bin,FieldSize*8,SampleCount,[]),
			Stz2 = #box_moov_trak_mdia_minf_stbl_stz2{version=Version,field_size=FieldSize,tz2=Tz2,sample_count=SampleCount},
			moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stz2=Stz2});
		{'stsc',<<Version,_:3/binary,EntryCount:32,Stz2Bin/binary>>,Rest} ->
			{ok,Tsc} = loop_analyse_stsc(Stz2Bin,EntryCount,[]),
			Stsc = #box_moov_trak_mdia_minf_stbl_stsc{version=Version,entry_count=EntryCount,entries=Tsc},
			moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stsc=Stsc});
		{'stco',<<Version,_:3/binary,EntryCount:32,Bin/binary>>,Rest} ->
			{ok,Tco} = loop_analyse_int_32(Bin,EntryCount,[]),
			Stco = #box_moov_trak_mdia_minf_stbl_stco{version=Version,entry_count=EntryCount,entries=Tco},
			moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stco=Stco});
		{'co64',<<Version,_:3/binary,EntryCount:32,Bin/binary>>,Rest} ->
			{ok,Offsets} = loop_analyse_int_64(Bin,EntryCount,[]),
			Co64 = #box_moov_trak_mdia_minf_stbl_co64{version=Version,entry_count=EntryCount,entries=Offsets},
			moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{co64=Co64});
		{'stss',<<Version,_:3/binary,EntryCount:32,Bin/binary>>,Rest} ->
%% 			?TRACK("stss bin:~p~n",[StssBin]),
			{ok,Tss} = loop_analyse_int_32(Bin,EntryCount,[]),
			Stss = #box_moov_trak_mdia_minf_stbl_stss{version=Version,entry_count=EntryCount,entries=Tss},
			moov_box_track_mida_minf_stbl(Rest, Stbl#box_moov_trak_mdia_minf_stbl{stss=Stss});
		{Type,Body,Rest} ->
			?INFO("~p -- ignore type:moov.trak.mdia.minf.stbl.~p size:~p~n",[?MODULE,Type,byte_size(Body)]),
			moov_box_track_mida_minf_stbl(Rest,Stbl);
		eof ->
			{ok,Stbl}
	end.

loop_stsd_analyse_entries(_,0,L) -> {ok,lists:reverse(L)};
loop_stsd_analyse_entries(<<>>,_,L) -> {ok,lists:reverse(L)};
loop_stsd_analyse_entries(R,EntryCount,L) ->
	case read_box_header(R) of
		%% AudioSampleEntry
		{'mp4a',<<_:6/binary,_DataRefIndex:16,_:8/binary,ChannelCount:16,SampleSize:16,PreDefined:16,_:2/binary,Samplerate:32,ESDSBin/binary>>,Rest} ->
			Audio = #stsd_audio_sample_entry{coding_name='mp4a',channel_count=ChannelCount,
									 samplerate=Samplerate,pre_defined=PreDefined,sample_size=SampleSize},
			{ok,Esds} = audio_esds(ESDSBin),
			loop_stsd_analyse_entries(Rest,EntryCount - 1,[Audio#stsd_audio_sample_entry{esds=Esds}|L]);
		{'avc1',<<_:6/binary,_DataRefIndex:16,_:16/binary,Width:16,Height:16,Horizre:32,Vertre:32,_:4/binary,FrameCount:16,_Compresorname:32/binary,
				  Depth:16,_:2/binary,RestBin/binary>>,Rest} ->
			H264 = avc1_avcc(RestBin),
			Video = #stsd_video_sample_entry{specific=H264,depth=Depth,frame_count=FrameCount,height=Height,width=Width,horizre=Horizre,vertre=Vertre},
			loop_stsd_analyse_entries(Rest,EntryCount-1,[Video|L]);
		{Type,Body,Rest} ->
			?INFO("~p -- ignore type:moov.trak.mdia.minf.stbl.entries[~p] size:~p~n",[?MODULE,Type,byte_size(Body)]),
			loop_stsd_analyse_entries(Rest,EntryCount-1,L);
		eof ->
			loop_stsd_analyse_entries(<<>>, EntryCount, L)
	end.

avc1_avcc(<<>>) -> undefined;
avc1_avcc(R) ->
	case read_box_header(R) of
		{'avcC',Body,_Rest} ->
%% 			?TRACK("avcc body:~p",[Body]),
			{ok,H264} = h264:decode_config(Body),
			H264;
		{Type,Body,Rest} ->
			?INFO("~p -- ignore type:moov.trak.mdia.minf.stbl.entries[avc1].~p size:~p~n",[?MODULE,Type,byte_size(Body)]),
			avc1_avcc(Rest);
		eof ->
			undefined
	end.

audio_esds(R) ->
	{'esds',<<Version:8,_Flag:3/binary,Bin/binary>>,_Rest} = read_box_header(R),
	{ok,analyse_stsd_audio_esds(Bin,#esds{version=Version})}.

analyse_stsd_audio_esds(Data, ESDS) -> 
  case mp4_read_discr_tag_data(Data) of
    {?MP4ESDescrTag, <<_ID1:16, _Priority1, Description/binary>>, <<>>} ->
    	analyse_stsd_audio_esds(Description, ESDS);
    {?MP4DecConfigDescrTag, <<ObjectType, StreamType:6,UpStream:1,_:1, BufferSize:24, MaxBitrate:32, AvgBitrate:32, Rest1/binary>>, Rest2} ->
		ESDS1 = ESDS#esds{object_type = mp4_object_type(ObjectType), stream_type = StreamType, buffer_size = BufferSize,
        							max_bitrate = MaxBitrate, avg_bitrate = AvgBitrate,upstream=UpStream},
      	ESDS2 = analyse_stsd_audio_esds(Rest1,ESDS1),
      	analyse_stsd_audio_esds(Rest2, ESDS2);
    {?MP4DecSpecificDescrTag, Config, Rest2} ->
		#esds{object_type=ObjectType}=ESDS,
      	ESDS1 = ESDS#esds{specific = analyse_audio_specific_config(ObjectType,Config)},
		analyse_stsd_audio_esds(Rest2, ESDS1);
    {?MP4Unknown6Tag, _Body, Rest} ->
      	analyse_stsd_audio_esds(Rest, ESDS);
    {_Tag, _Data, Rest} ->
      	analyse_stsd_audio_esds(Rest, ESDS);
    undefined ->
      	ESDS
  end.

analyse_audio_specific_config(aac,Config)->
	aac:decode_config(Config);
analyse_audio_specific_config(Type,Config)->
	?INFO("~p -- ignore type:moov.trak.mdia.minf.stbl.entries[mpa4].esds.specificConfig:~p size:~p",
		  [?MODULE,Type,byte_size(Config)]),
	undefined.
	

mp4_object_type(8) -> text;
mp4_object_type(16#20) -> mpeg4;
mp4_object_type(16#21) -> h264;
mp4_object_type(16#40) -> aac;
mp4_object_type(16#60) -> mpeg2video;
mp4_object_type(16#61) -> mpeg2video;
mp4_object_type(16#62) -> mpeg2video;
mp4_object_type(16#63) -> mpeg2video;
mp4_object_type(16#64) -> mpeg2video;
mp4_object_type(16#65) -> mpeg2video;
mp4_object_type(16#66) -> aac;
mp4_object_type(16#67) -> aac;
mp4_object_type(16#68) -> aac;
mp4_object_type(16#69) -> mp3;
mp4_object_type(16#6A) -> mpeg1video;
mp4_object_type(16#6B) -> mp3;
mp4_object_type(16#6C) -> mjpeg;
mp4_object_type(16#6D) -> png;
mp4_object_type(16#6E) -> jpeg2000;
mp4_object_type(16#A3) -> vc1;
mp4_object_type(16#A4) -> dirac;
mp4_object_type(16#A5) -> ac3;
mp4_object_type(16#DD) -> vorbis;
mp4_object_type(16#E0) -> dvd_subtitle;
mp4_object_type(16#E1) -> qcelp.

mp4_read_discr_tag_data(<<Tag,0:1, Length:7, Rest:Length/binary, Rest2/binary>>) ->
%% 	?TRACK("tag:~p,data:~p,rest:~p",[Tag,Rest, Rest2]),
  	{Tag,Rest, Rest2};
mp4_read_discr_tag_data(<<Tag,1:1, Length1:7, 0:1, Length:7, Rest/binary>>) ->
  	TagLength = Length1 bsl 7 + Length,
  	<<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  	{Tag,Rest1, Rest2};
mp4_read_discr_tag_data(<<Tag,1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  	TagLength = (Length2 bsl 14 + Length1 bsl 7 + Length),
  	<<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  	{Tag,Rest1, Rest2};
mp4_read_discr_tag_data(<<Tag,1:1, Length3:7, 1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  	TagLength = (Length3 bsl 21 + Length2 bsl 14 + Length1 bsl 7 + Length),
  	<<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  	{Tag,Rest1, Rest2};
mp4_read_discr_tag_data(<<>>) -> undefined.


loop_analyse_tsz_without_count(<<>>,SampleCount,L) -> {ok,SampleCount,lists:reverse(L)};
loop_analyse_tsz_without_count(<<Size:32,R/binary>>,SampleCount,L) ->
	loop_analyse_tsz_without_count(R, SampleCount + 1, [Size|L]).
	
loop_analyse_tsz(<<>>,_,L) -> {ok,lists:reverse(L)};
loop_analyse_tsz(_,0,L) -> {ok,lists:reverse(L)};
loop_analyse_tsz(<<Size:32,R/binary>>,SampleCount,L) ->
	loop_analyse_tsz(R, SampleCount - 1, [Size|L]).

loop_analyse_int_32(<<>>,_,L) -> {ok,lists:reverse(L)};
loop_analyse_int_32(_,0,L) -> {ok,lists:reverse(L)};
loop_analyse_int_32(<<ChunkOffset:32,R/binary>>, EntryCount, L) ->
	loop_analyse_int_32(R, EntryCount - 1, [ChunkOffset|L]).

loop_analyse_int_64(<<>>,_,L) -> {ok,lists:reverse(L)};
loop_analyse_int_64(_,0,L) -> {ok,lists:reverse(L)};
loop_analyse_int_64(<<ChunkOffset:64,R/binary>>, EntryCount, L) ->
	loop_analyse_int_64(R, EntryCount - 1, [ChunkOffset|L]).

loop_analyse_stsc(<<>>,_EntryCount,L) -> {ok,lists:reverse(L)};
loop_analyse_stsc(_,0,L) -> {ok,lists:reverse(L)};
loop_analyse_stsc(<<FirstChunk:32,SamplesPerChunk:32,SampleDescIndex:32,R/binary>>,EntryCount,L) ->
	loop_analyse_stsc(R,EntryCount - 1,[{FirstChunk,SamplesPerChunk,SampleDescIndex}|L]).

loop_analyse_stz2(<<>>,_FieldSize,_SampleCount,L) -> {ok,lists:reverse(L)};
loop_analyse_stz2(_,_,0,L) -> {ok,lists:reverse(L)};
loop_analyse_stz2(Rest,FieldSize,SampleCount,L) ->
	<<Size,FieldSize,R/binary>> = Rest,
	loop_analyse_stz2(R,FieldSize,SampleCount - 1,[Size|L]).

loop_anaylse_tts(<<>>,_,TTS) -> {ok,lists:reverse(TTS)};
loop_anaylse_tts(_,0,TTS) -> {ok,lists:reverse(TTS)};
loop_anaylse_tts(<<SampleCount:32,SampleDelta:32,Rest/binary>>,EntyCount,TTS) ->
	loop_anaylse_tts(Rest, EntyCount - 1, [{SampleCount,SampleDelta}|TTS]).
%% 	loop_anaylse_tts(Rest, EntyCount - 1, TTS).					  

loop_analyse_brands(<<>>,L) ->{ok,lists:reverse(L)};
loop_analyse_brands(<<Brands:4/binary,R/binary>>,L) ->
	loop_analyse_brands(R, [binary_to_list(Brands)|L]).

read_box_header(<<1:32,"uuid",LargeSize:64,TypeBin:16/binary,R/binary>>) ->
	Type = ?BIN_TYPE_TO_ATOM(TypeBin),
	BodySize = LargeSize - 32,
	<<Body:BodySize/binary,Rest/binary>> = R,
	{Type,Body,Rest};
read_box_header(<<0:32,"uuid",TypeBin:16/binary,Body/binary>>) ->
	Type = ?BIN_TYPE_TO_ATOM(TypeBin),
	{Type,Body,<<>>};
read_box_header(<<Size:32,"uuid",TypeBin:16/binary,R/binary>>) ->
	Type = ?BIN_TYPE_TO_ATOM(TypeBin),
	BodySize = Size - 24,
	<<Body:BodySize/binary,Rest/binary>> = R,
	{Type,Body,Rest};
read_box_header(<<1:32,TypeBin:4/binary,LargeSize:64,R/binary>>) ->
	Type = ?BIN_TYPE_TO_ATOM(TypeBin),
	BodySize = LargeSize - 16,
	<<Body:BodySize/binary,Rest/binary>> = R,
	{Type,Body,Rest};
read_box_header(<<0:32,TypeBin:4/binary,Body/binary>>) ->
	Type = ?BIN_TYPE_TO_ATOM(TypeBin),
	{Type,Body,<<>>};
read_box_header(<<Size:32,TypeBin:4/binary,R/binary>>) ->
	Type = ?BIN_TYPE_TO_ATOM(TypeBin),
	BodySize = Size - 8,
	<<Body:BodySize/binary,Rest/binary>> = R,
	{Type,Body,Rest};
read_box_header(<<>>) ->
	eof.

count_size([],S) -> S;
count_size([Size|T],S) ->
	count_size(T,Size + S).
