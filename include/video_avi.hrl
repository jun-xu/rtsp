

-define(AVI_RIFF,"RIFF").
-define(AVI_FLG,"AVI ").
-define(AVI_LIST,"LIST").
-define(AVI_LIST_FLAG,"hdrl").
-define(AVI_HEAD,"avih").
-define(AVI_SUB_LIST_FLAG,"strl").

-define(AVI_SUBL_LIST_FLAG_H,"strh").
-define(AVI_SUBL_LIST_FLAG_F,"strf").

-define(AVI_DATA_LIST,"LIST").
-define(AVI_DATA_LIST_FLAG,"movi").

-define(AVI_INDEX_LIST,"idx1").


-define(AVI_FCC_TYPE_VIDS,"vids").
-define(AVI_FCC_TYPE_AUDS,"auds").
-define(AVI_FCC_TYPE_MIDS,"mids").
-define(AVI_FCC_TYPE_TXTS,"txts").

-define(AVI_INDEX_TYPE_I,16#10).

-define(AVI_INDEX_FRAME_TYPE_AUDIO,"wb").
-define(AVI_INDEX_FRAME_TYPE_PC,"pc").

-define(AVI_VIDEO_BINARY_FLAGS,[<<"dc">>,<<"db">>]).
-record(track_info,{id = 0,
				type = "<<dc>>",
				iindexs = []}).

-record(index_info,{id= ?AVI_INDEX_LIST,
					size = 0
					}).


-record(avi_read_header,{fps = 40,
						 vids_fcc_handler = "H264",
						 vids_account = 0
						 }).



-record(state, {file_path,
				fd = undefined,
				read_start_pos = 0 :: integer(),  %% ms
				first_read = false,
				buff = undefined,
				avi_info = undefined :: undefined,
				data_start_offset = 0,
				cur_offset = 0,
				i_video_indexs = [],
				avi_read_header = undefined :: undefined | #avi_read_header{}
			   }).