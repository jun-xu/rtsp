%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-2-2
%%% -------------------------------------------------------------------
-module(rtsp_server_listener).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Port) ->
	gen_server:start_link(?MODULE, [Port], []).	

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Port]) ->
	{ok,ListenSocket} = gen_tcp:listen(Port, [binary, {reuseaddr, true},{active,false},{backlog, 1000}]),	
	gen_server:cast(self(), start_loop),
	?INFO("~p -- start on port:~p",[?MODULE,Port]),
    {ok, #state{lsock=ListenSocket}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: endless loop 
%% --------------------------------------------------------------------
handle_cast(start_loop, #state{lsock=ListenSocket}) ->
	loop(ListenSocket).

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
loop(ListenSocket) ->
	{ok,Socket} = gen_tcp:accept(ListenSocket),
	{ok,Pid} = rtsp_client_sup:start_child(),
	gen_tcp:controlling_process(Socket, Pid),
%% 	gen_server:cast(Pid,{socket_ready,Socket}),
	rtsp_socket:set_socket(Pid, Socket),
	loop(ListenSocket).