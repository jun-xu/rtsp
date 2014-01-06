%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2014-1-2
%%% -------------------------------------------------------------------
-module(mp4_to_h264).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").
-include("h264.hrl").
-include("aac.hrl").
-include("media_info.hrl").
-include("rtsp.hrl").
-include("rtp_session.hrl").
-include("video_file.hrl").
-include("video_mpg4.hrl").
-include_lib("kernel/include/file.hrl").

-define(FILE_WRITE_OPEN_OPTIONS,[write,read,raw,binary]).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,stop/1,to_h264/2]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link(?MODULE, [], []).

to_h264(Pid,FilePath) ->
	gen_server:call(Pid, {init_file,FilePath},infinity).

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
handle_call({init_file,FilePath},_,State) ->
	RootName = filename:rootname(FilePath),
	case prim_file:open(FilePath, ?FILE_READ_OPEN_OPTIONS) of
		{ok,FD} ->
			prim_file:delete(RootName++".h264"),
			case prim_file:open(RootName++".h264",?FILE_WRITE_OPEN_OPTIONS) of
				{ok,WFD} ->
					ok = mp4_to_h264(FilePath,FD,WFD),
					prim_file:close(WFD),
					prim_file:close(FD),
					{reply, ok, State};
				{error,R} ->
					{reply, {error,R}, State}
			end;
		{error,R} ->
			{reply, {error,R}, State}
	end;
handle_call(stop, From, State) ->
	{stop, normal, ok, State};
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
write_h264_frame(FD,#h264{sps=SPS,pps=PPS}) ->
	prim_file:write(FD, <<0,0,0,1,SPS/binary>>),
	prim_file:write(FD, <<0,0,0,1,PPS/binary>>);
write_h264_frame(_FD,[]) -> ok;
write_h264_frame(_FD,[eof]) -> eof;
write_h264_frame(FD,[#frame{data=Data}|T]) ->
	prim_file:write(FD, <<0,0,0,1,Data/binary>>),
	write_h264_frame(FD, T).
	
mp4_to_h264(FilePath,FD,WFD) ->
	{ok,Mp4Info} = mp4_util:decode_mpg4_file(FilePath, FD),
	{ok,_,[{_,TrackIndex=#mpg4_index{specific=H264}}]} = mp4_util:analyze_media_info(Mp4Info),
	{ok,#mpg4_reader{cur_offset=Offset} = Reader} = mp4_util:position(TrackIndex,0),
	{ok,_} = prim_file:position(FD, Offset),
	ok = write_h264_frame(WFD,H264),
	ok = loop_write_h264(?DEFAULT_MIN_READ_FRAMES_Length,WFD,Reader#mpg4_reader{fd=FD}),
	ok.

loop_write_h264(ReaderLength,WFD,Reader) ->
	case mp4_util:read_frames(ReaderLength,Reader,[]) of
		{ok,NewReader,Frames} ->
			?TRACK("~p -- read frames:~p",[?MODULE,length(Frames)]),
			ok = write_h264_frame(WFD,lists:reverse(Frames)),
			loop_write_h264(ReaderLength,WFD,NewReader);
		{eof,Frames} ->
			?TRACK("~p -- read frames:~p eof.",[?MODULE,length(Frames)]),
			eof = write_h264_frame(WFD,lists:reverse(Frames)),
			ok;
		{error,R} ->{error,R}
	end.
	
	
