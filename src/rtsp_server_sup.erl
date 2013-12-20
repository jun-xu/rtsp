
-module(rtsp_server_sup).

-include("log.hrl").
-include("rtsp.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	?INFO("~p -- start...~n",[?MODULE]),
	RtspPort= rtsp_util:get_app_env(rtsp_port,[],?DEFAULT_LISTEN_PORT),
    RtspServer = {rtsp_server, {rtsp_server_listener,start_link,[RtspPort]},
				  permanent,2000,worker,[rtsp_server_listener]},
    RtspSocketSup = {rtsp_socket_sup,{rtsp_client_sup, start_link, []},
					 permanent,infinity,supervisor,[rtsp_socket_sup]},	
	RootDirs = {root_dir_manager, {root_dir_manager,start_link,[]},
				  permanent,2000,worker,[root_dir_manager]},
	
    {ok, {{one_for_one, 5, 10}, [RootDirs,RtspServer, RtspSocketSup]}}.

	
	
	
	
