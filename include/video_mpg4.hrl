-define(BIN_TYPE_TO_ATOM(TypeBin),binary_to_atom(TypeBin,latin1)).

-define(BOX_FTYP,"ftyp").
-define(BOX_UUID,'uuid').
-define(BOX_MOOV,"moov").

-define(BOX_MOOV_MVHD,'mvhd').
-define(BOX_MOOV_TRAK,'trak').
-define(BOX_MOOV_TRAK_TKHD,'tkhd').
-define(BOX_MOOV_TRAK_MDIA,'mdia').
-define(BOX_MOOV_TRAK_MDIA_MDHD,'mdhd').
-define(BOX_MOOV_TRAK_MDIA_HDLR,'hdlr').
-define(BOX_MOOV_TRAK_MDIA_MINF,'minf').

-define(TRAK_TYPE_VIDEO,'vide').
-define(TRAK_TYPE_AUDIO,'soun').
-define(TRAK_TYPE_HINT,'hint').
%% from ISO/IEC 14496-3
-define(Audie_GA_SPECIFIC_CONF_LIST,[1,2,3,4,6,7,17,19,20,21,22,23]).

%% from  ISO/IEC 14496-1
-define(MP4ESDescrTag, 3).
-define(MP4DecConfigDescrTag, 4).
-define(MP4DecSpecificDescrTag, 5).
-define(MP4Unknown6Tag, 6).

-record(esds, { version,
				object_type,
  				stream_type,
  				upstream,
  				buffer_size,
  				max_bitrate,
  				avg_bitrate,
  				specific
				}).

%% from  ISO/IEC 14496-12
%% avc1
-record(stsd_video_sample_entry,{width,
								 height,
								 horizre,
								 vertre,
								 frame_count = 1,
								 depth = 16#18,
								 specific
								 }).
%% mp4a
-record(stsd_audio_sample_entry,{coding_name,
								 channel_count,
								 sample_size,
								 pre_defined,
								 samplerate,
								 esds :: #esds{}		
								 }).

-record(box_moov_trak_mdia_minf_stbl_stsd,{version = 0,
			  							   entries = [] :: [#stsd_video_sample_entry{} | #stsd_audio_sample_entry{}]
			 								}).

-record(box_moov_trak_mdia_minf_stbl_stts,{entiy_count = 0,
										   version = 0,
										   tts = [] :: [{SampleCount::integer(),SampleDelta::integer()}]
										  }).
-record(box_moov_trak_mdia_minf_stbl_stsz,{version = 0,
										   sample_size = 0,
										   sample_count = 0,
										   tsz = [] :: [SampleSize::integer()]
										  }).
-record(box_moov_trak_mdia_minf_stbl_stz2,{version = 0,
										   sample_count = 0,
										   field_size = 4,
										   tz2 = [] :: [SampleSize::integer()]
										  }).

-record(box_moov_trak_mdia_minf_stbl_stsc,{version = 0,
										   entry_count = 0,
										   entries = [] :: [{FirstChunk::integer(),SamplesPerChunk::integer(),SampleDescIndex::integer()}]
										   }).
-record(box_moov_trak_mdia_minf_stbl_stco,{version = 0,
										   entry_count = 0,
										   entries = [] :: [ChunkOffset::integer()]
										   }).

-record(box_moov_trak_mdia_minf_stbl_co64,{version = 0,
										   entry_count = 0,
										   entries = [] :: [ChunkOffset::integer()]
										   }).

-record(box_moov_trak_mdia_minf_stbl_stss,{version = 0,
										   entry_count = 0,
										   entries = [] :: [SampleNumber::integer()]}).

%% stbl
-record(box_moov_trak_mdia_minf_stbl,{stts :: #box_moov_trak_mdia_minf_stbl_stts{},  	%% time to sample
									  stsd :: #box_moov_trak_mdia_minf_stbl_stsd{},		%% 
									  stsz :: #box_moov_trak_mdia_minf_stbl_stsz{},		%% sample size
									  stz2 :: #box_moov_trak_mdia_minf_stbl_stz2{},		%% sample size
									  stsc :: #box_moov_trak_mdia_minf_stbl_stsc{},     %% sample to trunk
									  stco :: #box_moov_trak_mdia_minf_stbl_stco{},		%% trunk offset 32.
									  co64 :: #box_moov_trak_mdia_minf_stbl_co64{},		%% trunk offset 64.
									  stss :: #box_moov_trak_mdia_minf_stbl_stss{}		%% sync sample (I index)
									  }).

%% video
-record(box_moov_trak_mdia_minf_vmhd,{version,
									  graphicsmode =0,
									  opcolor = {0,0,0}}).
%% sound
-record(box_moov_trak_mdia_minf_smhd,{version,
									  balance}).
%% hit
-record(box_moov_trak_mdia_minf_hmhd,{version,
									  maxPDUsize,
									  avgPDUsize,
									  maxbitrate,
									  avgbitrate}).
%% null
-record(box_moov_trak_mdia_minf_nmhd,{version,
									  flag}).
								

-record(box_moov_trak_mdia_minf,{xmhd :: #box_moov_trak_mdia_minf_vmhd{}|#box_moov_trak_mdia_minf_smhd{}|
										 #box_moov_trak_mdia_minf_hmhd{}|#box_moov_trak_mdia_minf_nmhd{},
								 stbl :: #box_moov_trak_mdia_minf_stbl{}
								 }).

-record(box_moov_trak_mdia_hdlr,{version = 0,
								 handler_type,	%% "vide"|"soun"|"hint"
								 name
								 }).

-record(box_moov_trak_mdia_mdhd,{
						version = 0,
						creation_time,
					  	modify_time,
					   	timescale,			%% is an integer that declares length of the presentation (in the indicated timescale)
					   	duration,
						language			%% declares the language code for this media.
						}).

-record(box_moov_trak_mdia,{mdhd :: #box_moov_trak_mdia_mdhd{},
							hdlr :: #box_moov_trak_mdia_hdlr{},
							minf :: #box_moov_trak_mdia_minf{}
							}).

-record(box_moov_trak_tkhd,{track_id,
					   		creation_time,
					   		modify_time,
					   		duration,
					   		layer,			%% specifies the front-to-back ordering of video tracks
							alter_group = 0,%% is an integer that specifies a group or collection of tracks
					   		volume,
					   		matrix,
					   		width,			%% specify the track's visual presentation size as fixed-point 16.16 values.
					   		height			%% specify the track's visual presentation size as fixed-point 16.16 values.
						   }).

-record(box_moov_trak,{tkhd :: #box_moov_trak_tkhd{},
					   mdia	:: #box_moov_trak_mdia{},	
					   rest_boxes = []
					   }).
-record(box_moov_mvhd,{version = 0,
					   id = ?BOX_MOOV_MVHD,
					   creation_time,
					   modify_time,
					   timescale,			%% is an integer that declares length of the presentation (in the indicated timescale)
					   duration,	
					   rate = 10000,		%% is a fixed point 16.16 number that indicates the preferred rate to play the presentation;
					   volume,				%% is a fixed point 8.8 number that indicates the preferred playback volume.
					   matrix,				%% provides a transformation matrix for the video; (u,v,w) are restricted here to (0,0,1), hex values 
											%% (0,0,0x40000000).
					   pre_defined = 0,
					   next_track_id
					   }).
-record(rest_box,{id,
				  size,
				  bin}).

-record(box_moov,{id=?BOX_MOOV,
				  version = 0,
				  mvhd,
				  traks=[] :: {TrackId::integer(),#box_moov_trak{}},
				  rest_boxes = [] :: [#rest_box{}]
				  }).

-record(box_ftyp,{id=?BOX_FTYP,			
				  major_brand,				%% is a brand identifier
				  minor_version,			%% is an informative integer for the minor version of the major brand
				  compatible_brands = []}).	%% is a list, to the end of the box, of brands


-record(mp4_media,{ftyp :: #box_ftyp{},
				   moov :: #box_moov{}
				   }).

-record(mpg4_reader,{timescale = 90000,
					 cur_offset = 0,
					 cur_duration = 0,
					 samples = [],
					 type = h264,
					 track = 1
					 }).

-record(mpg4_index,{timescale = 90000,
					samples :: [{Index::integer(),Size::integer(),Duration::integer()}|{trunk,Offset::integer()}],
					stss
					 }).

-record(mpg4_state,{file_path,
					read_start_pos = 0 :: integer(),  %% ms
					first_read = false,
					buff = undefined,
					mp4_media = undefined,
					media_info,
					data_start_offset = 0,
					indexes = [] ::[{TrackId::integer(),Index::#mpg4_index{}}],
					readers = [] ::[{TrackId::integer(),FD::any(),Reader::#mpg4_reader{}}]
					}).


