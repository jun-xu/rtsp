%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2013-12-24
%%% -------------------------------------------------------------------
-module(file_mpg4_reader).

-behaviour(gen_server).
-behaviour(rtsp_source_reader).
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

position(Pid,StartPos,Ignore) ->
	gen_server:call(Pid, {position,StartPos,Ignore},infinity).

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
    {ok, #mpg4_state{}}.

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
	{reply, ok, State#mpg4_state{file_path=FilePath}};	

handle_call(get_sdp,_,#mpg4_state{file_path=FilePath,media_info=undefined} = State) ->
	{ok,FD} = prim_file:open(FilePath,?FILE_READ_OPEN_OPTIONS),
	{ok,Mp4Info} = mp4_util:decode_mpg4_file(FilePath, FD),
%% 	?TRACK("mp4Info:~p",[Mp4Info]),
	{ok,MediaInfo,Indexes} = mp4_util:analyze_media_info(Mp4Info),
%% 	?INFO("~p -- indexes:~p",[?MODULE,Indexes]),
	prim_file:close(FD),
	{reply, {ok,MediaInfo}, State#mpg4_state{media_info=MediaInfo,indexes=Indexes}};
handle_call(get_sdp,_,#mpg4_state{media_info=MediaInfo} = State) ->
	{reply, {ok,MediaInfo}, State};

handle_call({position,_,_},_,#mpg4_state{indexes=[]} = State) ->
	?INFO("~p -- no iindex to position.",[?MODULE]),
	{reply, ok, State#mpg4_state{first_read=true}};
handle_call({position,StartPos,_},_,#mpg4_state{readers=Readers,file_path=FilePath,indexes=Indexes} = State) ->
	ok = loop_close_readers(Readers),
	{ok,NewReaders} = loop_position_reader(FilePath,StartPos,Indexes,[]),
	{reply,ok,State#mpg4_state{readers=NewReaders,first_read=true}};

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
handle_cast({read,From,ReadId},#mpg4_state{readers=[]} = State) ->
	ok = callback_to(From,ReadId,[eof]),
	{noreply, State};
handle_cast({read,From,ReadId},#mpg4_state{readers=Readers,first_read=FirstRead} = State) ->
	RealReadLength = case FirstRead of
						 true ->
							?DEFAULT_MIN_READ_FRAMES_Length div 2;
						 false ->
						 	?DEFAULT_MIN_READ_FRAMES_Length
					 end,
	case loop_read_frames(Readers,RealReadLength,[],From,ReadId,FirstRead) of
		{ok,NewReaders} ->
			{noreply,State#mpg4_state{readers=NewReaders,first_read=false}};		
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
terminate(Reason, State) ->
	?INFO("~p -- terminate by reason:~p",[?MODULE,Reason]),
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

loop_position_reader(_,_,[],L) -> {ok,lists:reverse(L)};
loop_position_reader(FilePath,StartPos,[{Track,TrackIndex}|T],L) ->
	{ok,#mpg4_reader{cur_offset=Offset} = Reader} = mp4_util:position(TrackIndex,StartPos),
	{ok,FD} = prim_file:open(FilePath,?FILE_READ_OPEN_OPTIONS),
	{ok,_} = prim_file:position(FD, Offset),
	?INFO("~p -- positin:~p offset:~p",[?MODULE,StartPos,Offset]),
	loop_position_reader(FilePath,StartPos,T,[{Track,Reader#mpg4_reader{fd=FD}}|L]).

loop_close_readers([]) -> ok;
loop_close_readers([{_,#mpg4_reader{fd=FD}}|T]) ->
	prim_file:close(FD),
	loop_close_readers(T).

loop_read_frames([],_,Readers,_From,_ReadId,_) -> {ok,Readers};
loop_read_frames([{Track,Reader}|T], RealReadLength, Readers,From,ReadId,FirstRead) ->
	case mp4_util:read_frames(RealReadLength,Reader,[]) of
		{ok,NewReader,Frames} ->
			?TRACK("read frames:~p",[length(Frames)]),
			callback_to(From,ReadId,Frames),
			loop_read_frames(T, RealReadLength, [{Track,NewReader}|Readers],From,ReadId,FirstRead);
		{eof,Frames} ->
			callback_to(From,ReadId,Frames),
			loop_read_frames(T, RealReadLength, Readers,From,ReadId,FirstRead);
		{error,_} ->
			loop_read_frames(T, RealReadLength, Readers,From,ReadId,FirstRead)
	end.

