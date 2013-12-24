%% Author: sunshine
%% Created: 2013-12-17
%% Description: TODO: Add description to sdp
-module(sdp).

%%
%% Include files
%%
-include("log.hrl").
-include("sdp.hrl").
-include("media_info.hrl").

-define(LSEP, <<$\r,$\n>>).

%%
%% Exported Functions
%%
-export([make_session/0,encode/1]).

%%
%% API Functions
%%

-spec make_session() -> string().
make_session() ->
  random:seed(now()),
  M = 100000000000000,
  integer_to_list(M+random:uniform(M)*5).

to_fmtp(h264, Config) -> h264:to_fmtp(Config);
%% to_fmtp(aac, Config) -> aac:to_fmtp(Config);
to_fmtp(_, _) -> undefined.

encode(#stream_info{content = Content, codec = Codec, stream_id = Id, options = Options, timescale = Timescale, params = Params} = Stream) ->

  Cliprect = case Params of
    #video_params{width = Width, height = Height} when Width >= 0 andalso Height >= 0 ->
      io_lib:format("a=cliprect:0,0,~p,~p\r\na=framesize:~p ~p-~p\r\na=x-dimensions:~p,~p\r\n", [Width, Height, payload_type(Codec),Width, Height,Width,Height]);
    _ -> ""
  end,
  SR = case Params of
         #audio_params{channels = _, sample_rate = SampleRate} -> SampleRate;
         _ -> Timescale
       end,
  Control = proplists:get_value(control, Options, io_lib:format("~s=~p", [?TRACK_TAG,Id])),
  FMTP = case to_fmtp(Codec, Options) of
    undefined -> "";
    Else -> io_lib:format("a=fmtp:~p ~s\r\n", [payload_type(Codec), Else])
  end,
  SDP = [
    io_lib:format("m=~s ~p RTP/AVP ~p", [Content, proplists:get_value(port, Options, 0), payload_type(Codec)]), ?LSEP,
    "a=control:", Control, ?LSEP,
    io_lib:format("a=rtpmap:~p ~s/~p", [payload_type(Codec), codec_to_sdp(Codec), SR]), additional_codec_params(Stream), ?LSEP,
    Cliprect,
    FMTP
  ],
  iolist_to_binary(SDP);
encode(#media_info{video = Video, audio = Audio, options = Options} = MediaInfo) ->
  iolist_to_binary([
    encode_sdp_session(proplists:get_value(sdp_session, Options)),
    encode_media_info(MediaInfo),
    [encode(V) || V <- Video],
    [encode(A) || A <- Audio]
  ]).


%%
%% Local Functions
%%
net(inet6) -> "IN IP6";
net(_) -> "IN IP4".

encode_sdp_session(#sdp_session{
  name = SessName,
  connect = {ConnectNet, ConnectAddr},
  originator = #sdp_o{
    username = User,
    sessionid = SessionId,
    version = SessVersion,
    netaddrtype = NetType,
    address = OriginAddr
  }, attrs = Attrs} = Sess) ->
  [
    "v=", integer_to_list(Sess#sdp_session.version), ?LSEP,
    io_lib:format("o=~s ~s ~s ~s ~s", [User, SessionId, SessVersion, net(NetType), OriginAddr]), ?LSEP,
    io_lib:format("s=~s", [SessName]), ?LSEP,
    io_lib:format("c=~s ~s", [net(ConnectNet), ConnectAddr]), ?LSEP,
    "t=0 0", ?LSEP,
    encode_attrs(Attrs)
  ].
encode_attrs(Attrs) ->
  lists:map(fun
    ({K, V}) -> io_lib:format("a=~s:~s\r\n", [K,V]);
    (K) when is_atom(K) -> io_lib:format("a=~s\r\n", [K])
  end, Attrs).


encode_media_info(#media_info{duration = Duration,options=Options}) when Duration =/= undefined ->
	Url = proplists:get_value(url, Options,"*"),
  [
    "a=control:",Url, ?LSEP,
    "a=range:npt=0-", case Duration of undefined -> ""; _ -> io_lib:format("~f", [Duration / 1000]) end, ?LSEP
  ];

encode_media_info(_) ->
  [].


additional_codec_params(#stream_info{content = audio, params = #audio_params{channels = Channels}}) ->
  "/"++integer_to_list(Channels);

additional_codec_params(_) ->
  "".

codec_to_sdp(Codec) -> 
	proplists:get_value(Codec, ?SDP_CODEC).


payload_type(h264) -> 96;
payload_type(h263) -> 98;
payload_type(aac) -> 97;
payload_type(pcma) -> 8;
payload_type(pcmu) -> 0;
payload_type(g726_16) -> 99;
payload_type(speex) -> 100;
payload_type(Codec) ->
	?INFO("~p -- unknow codec:~p",[?MODULE,Codec]),
	97.