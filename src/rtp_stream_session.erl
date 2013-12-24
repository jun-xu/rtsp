%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2013-12-20
%%% -------------------------------------------------------------------
-module(rtp_stream_session).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rtp_session.hrl").
-include("sdp.hrl").
-include("media_info.hrl").
-include("video_stream.hrl").
-include("rtsp.hrl").
-include("log.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,setup/2,play/2,pause/2,get_sdp/2,register_read/3]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec register_read(Pid::pid(),Mod::atom(),Reader::pid()) -> ok | {error,Reason::any()}.
register_read(Pid,Mod,Reader) ->
	gen_server:call(Pid, {register_read,Mod,Reader},infinity).

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
    {ok, #rtp_session_streams{}}.

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
handle_call({register_read,Mod,ReaderPid},_,#rtp_session_streams{source_pid=undefined} = State) ->
	Ref = erlang:monitor(process, ReaderPid),
	{reply, ok, State#rtp_session_streams{source_ref=Ref,source_pid={Mod,ReaderPid}}};
handle_call({register_read,_,ReaderPid},_,#rtp_session_streams{source_pid={_,ReaderPid}} = State) ->
	{reply, ok, State};
handle_call({register_read,_,_},_,State) ->
	{reply, {error,already_register}, State};

handle_call({get_sdp,_},_,#rtp_session_streams{source_pid=undefined} = State) ->
	?INFO("~p -- no media source to read sdp.",[?MODULE]),
	{reply, {error,cannot_find}, State};
handle_call({get_sdp,URL},_,#rtp_session_streams{sdp=undefined,source_pid={Mod,ReaderPid}} = State) ->
	case decode_stream_path(URL) of
		{ok,StreamId} ->
			case Mod:get_sdp(ReaderPid) of
				{ok,#media_info{options=Options} =MediaInfo} ->
					{ok,SessionId,NewMediaInfo} = rtsp_util:gen_sdp_session(MediaInfo#media_info{options=[{url,URL}|Options]}),
					Sdp = sdp:encode(NewMediaInfo),
					?INFO("~p -- init sdp:~p~nsession:~p",[?MODULE,Sdp,SessionId]),
					{reply, {ok,Sdp,SessionId}, State#rtp_session_streams{sdp=Sdp,session_id=SessionId,media_info=NewMediaInfo,source_pid=ReaderPid}};
				E ->
					?INFO("~p -- get sdp of stream:~p error:~p",[?MODULE,StreamId,E]),
					{stop, close, {error, cannot_find}, State}
			end;
		{error,Reason} ->
			?INFO("~p -- bad request:~p by reason:~p",[?MODULE,URL,Reason]),
			{stop, close, {error, bad_request}, State}
	end;
handle_call({get_sdp,_URL},_,#rtp_session_streams{sdp=Sdp,session_id=SessionId} = State) ->
	{reply, {ok,Sdp,SessionId}, State};		

handle_call(#setup{url=Url} = Setup, _From, #rtp_session_streams{channels=Channels} = State) ->
	?INFO("~p -- udp setup:~p~n",[?MODULE,Setup]),
	case parse_track(Url) of
		{error,not_found} -> 
			{reply, {error, bad_request}, State};
		{ok,Track} ->
			case proplists:get_value(Track, Channels) of
				{SenderPid} ->
					case rtp_stream_udp_sender:setup(SenderPid,Setup) of
						{ok,ServerPorts} -> {reply,{ok,ServerPorts},State};
						{error,Reason} ->
							?INFO("~p -- setup channel of track:~p error by reason:~p",[?MODULE,Track,Reason]),
							{reply, {error, bad_request}, State}
					end;
				undefined ->
					{ok,SenderPid} = rtp_stream_udp_sender:start_link(),
					case rtp_stream_udp_sender:setup(SenderPid,Setup) of
						{ok,ServerPorts} -> 
							{reply,{ok,ServerPorts},State#rtp_session_state_ex{channels=[{Track,SenderPid}|Channels]}};
						{error,Reason} ->
							?INFO("~p -- setup channel of track:~p error by reason:~p",[?MODULE,Track,Reason]),
							{reply, {error, bad_request}, State}
					end
			end
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
handle_info({'DOWN',Ref,process,_Pid,_Info},#rtp_session_streams{source_ref=Ref,source_pid=SourcePid} = State) ->
	?INFO("~p -- source:~p down,stop.~n",[?MODULE,SourcePid]),
%% 	catch release_all_clients(Clients,source_down),
	{stop, normal, State#rtp_session_streams{source_pid=undefined,source_ref=undefined}};
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
decode_stream_path(Url) ->
	case re:run(Url, ".*"++ ?STEAMS_PRE_FIX ++"/(.*)", [{capture, all_but_first, list}]) of
		nomatch -> {error,nomatch};
		{match,[StreamId]} ->
			{ok,StreamId}
	end.

parse_track(Url) ->
	case re:run(Url, ".*"++ ?STEAMS_PRE_FIX ++".*/"++?TRACK_TAG++"=(.*)", [{capture, all_but_first, list}]) of
		nomatch -> {error,not_found};
		{match,[Track]} ->
			{ok,list_to_integer(Track)}
	end.
