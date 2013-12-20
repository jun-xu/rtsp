%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2013-12-13
%%% -------------------------------------------------------------------
-module(root_dir_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,stop/0,get_file_path/1,add_root_dir/1]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {roots = []}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

get_file_path(SubFilePath) ->
	gen_server:call(?MODULE, {file_path,SubFilePath},infinity).

add_root_dir(Dir) ->
	gen_server:call(?MODULE, {add_root_dir,Dir},infinity).

stop() ->
	gen_server:call(?MODULE, stop).

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
	Roots = rtsp_util:get_app_env(roots, [], []),
	?INFO("~p -- start roots:~p",[?MODULE,Roots]),
    {ok, #state{roots=Roots}}.

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
handle_call({add_root_dir,Dir},_,#state{roots=Roots} = State) ->
	?INFO("~p -- add root dir:~p",[?MODULE,Dir]),
	case lists:member(Dir, Roots) of
		true -> {reply, ok, State};
		false -> {reply, ok, State#state{roots=[Dir|Roots]}}
	end;
handle_call({file_path,SubFilePath},_,#state{roots=Roots} = State) ->
	case search_file(SubFilePath,Roots) of
		{error,Reason} ->
			?INFO("~p -- search file:~p error:~p~nroots:~p",[?MODULE,SubFilePath,Reason,Roots]),
			{reply,{error,Reason},State};
		{ok,FilePath} ->
			{reply,{ok,FilePath},State}
	end;

handle_call(stop, _From, State) ->
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
search_file(_,[]) -> {error,not_found};
search_file(SubFilePath,[Root|T]) ->
	Filepath = lists:concat([Root,SubFilePath]),
	case filelib:is_file(Filepath) of
		true ->
			{ok,Filepath};
		false ->
			search_file(SubFilePath,T)
	end.
