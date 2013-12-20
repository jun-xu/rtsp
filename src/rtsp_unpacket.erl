%% Author: Administrator
%% Created: 2012-2-15
%% Description: TODO: Add description to packet_codecc
-module(rtsp_unpacket).


%%
%% Include files
%%
-include("log.hrl").
%%
%% Exported Functions
%%
-export([decode/1]).
-export([decode_range_value/1]).

parse(ready, Data) ->		
    case erlang:decode_packet(line, Data, []) of
		{ok, Line, Rest}->
			case re:compile("([^ ]+)\s+([^ ]+)\s+(RTSP/1\\.0)") of			
				{ok,Re} ->
					case re:run(Line, Re, [{capture,[1,2,3],binary}]) of
						{match,[Method,URL,<<"RTSP/1.0">>]} ->
							{ok, {rtsp_request, binary_to_atom(Method, latin1), URL}, Rest}
           			end
			end;
		_ ->
			{more, ready, Data}
	end;

parse(header,Data) ->
	case erlang:decode_packet(httph_bin, Data, []) of
		{ok,{http_header, _, Key, _, Value},Rest} ->
			{ok,{header, Key, Value}, Rest};
		{ok, http_eoh, Rest} ->
			{ok, header_end, Rest};
		{more, undefined} ->
			{more, header, Data}
	end;

parse({body, Length}, Data) when byte_size(Data) >= Length ->
  {Body, Rest} = split_binary(Data, Length),
  {ok, {body, Body}, Rest};

parse({body, _Length}, Data) ->
  {more, body, Data}.

decode_headers(Data, Headers, BodyLength) ->
  case parse(header, Data) of

%%     {ok, {header, 'Content-Length', Length}, Rest} ->
%%       NewLength = list_to_integer(binary_to_list(Length)),
%%       decode_headers(Rest, [{'Content-Length', NewLength} | Headers], NewLength);
    {ok, {header, HKey, HVal}, Rest} ->
      NewPair =
        case HKey of
          <<"Cseq">> -> {'Cseq', HVal};
          <<"Transport">> -> {'Transport', parse_transport_header(HVal)};
          <<"Session">> -> {'Session', HVal};
          %<<"Call-Id">> -> {'Call-Id', HVal};
          %<<"User-Agent">> -> {'User-Agent', HVal};
          %<<"Accept">> -> {'Accept', HVal};
          %<<"Range">> -> {'Range', HVal};
		   <<"Scale">> -> {'Scale', HVal};
		   <<"File_id">> -> {'File_id',list_to_integer(binary_to_list(HVal))};			%% for change fileid dynamic
          _ -> 
%			  case erlang:is_binary(HKey) of
%				   true ->
%			  		{binary_to_atom(HKey, latin1), HVal};
%				  false ->
					  {HKey, HVal}
%			  end
        end,
     	decode_headers(Rest, [NewPair | Headers], BodyLength);

%%     {ok, header_end, Rest} when BodyLength == undefined ->
%%       {ok, Headers, undefined, Rest};
    {ok, header_end, Rest} ->
	  	case proplists:get_value('Content-Length', Headers) of
		  	undefined ->
			  	{ok, Headers, undefined, Rest};
		  	LengthBin ->
			  	NewBodyLength = list_to_integer(binary_to_list(LengthBin)),
			  	case parse({body, NewBodyLength}, Rest) of
        			{ok, {body, Body}, Rest1} ->
          				{ok, Headers, Body, Rest1};
        			{more, body, _} ->
         		 		more
      		  	end
	  end;
      


    {more, header, Data} ->
      more
  end.
	
parse_transport_header(Header) ->
  Fields = lists:foldl(fun
    	("interleaved="++Interleaved, Opts) ->
      		[Chan0, Chan1] = string:tokens(Interleaved, "-"),
     		[{interleaved, {list_to_integer(Chan0), list_to_integer(Chan1)}}|Opts];
    	("RTP/AVP/TCP", Opts) -> [{proto, {tcp,"RTP/AVP/TCP"}}|Opts];
    	("RTP/AVP/UDP", Opts) -> [{proto, {udp,"RTP/AVP/UDP"}}|Opts];
    	("RTP/AVP", Opts) -> [{proto, {udp,"RTP/AVP"}}|Opts];
		("RTP/AVT", Opts) -> [{proto, {udp,"RTP/AVT"}}|Opts];
%    	("mode=record", Opts) -> [{mode, 'receive'}|Opts];
%    	("mode=receive", Opts) -> [{mode, 'receive'}|Opts];
%    	("mode=\"PLAY\"", Opts) -> [{mode, play}|Opts];
    	("unicast", Opts) -> [{unicast, true}|Opts];
%    	("source="++Source, Opts) -> [{source, Source}|Opts];
    	("client_port="++Ports, Opts) ->
      		[Port0,Port1] = string:tokens(Ports, "-"),
      		[{client_port, {list_to_integer(Port0),list_to_integer(Port1)}}|Opts]
%    	("server_port="++Ports, Opts) ->
%      		[Port0,Port1] = string:tokens(Ports, "-"),
%      		[{server_port, {list_to_integer(Port0),list_to_integer(Port1)}}|Opts];
%    	("ssrc="++SSRC, Opts) -> [{ssrc, erlang:list_to_integer(SSRC, 16)}|Opts];
%    	(Else, Opts) -> Parts = string:tokens(Else, "="), [{hd(Parts),string:join(tl(Parts),"=")}|Opts]
  end, [], string:tokens(binary_to_list(Header), ";")),
  lists:reverse(Fields).

decode(Data) ->
	case parse(ready,Data) of
		{more, ready, Data} ->
			{more,Data};
		{ok, {rtsp_request, Method, URL}, Rest} ->
			case decode_headers(Rest, [],undefined) of
				more ->
					{more,Data};
				{ok, Headers, Body,Rest1} ->
					{ok,{request,Method,URL,lists:ukeysort(1, Headers),Body},Rest1}
			end
		end.
	
decode_range_value(Data) ->
	case re:compile("npt=([0-9.]+)-([0-9.]*)") of
		{ok,Re} ->
			case re:run(Data, Re,[{capture,[1,2],binary}]) of
				{match,[BTime, <<>>]} ->
					{ok, binary_to_millisecond(BTime), undefined};
				{match,[BTime, ETime]} ->
					{ok, binary_to_millisecond(BTime), binary_to_millisecond(ETime)}
			end
	end.

binary_to_millisecond(Data) ->
	case re:compile("([0-9]+)[.]?([0-9]{1,3})?") of
		{ok, Re} ->
			case re:run(Data, Re, [{capture, all_but_first, list}]) of
				{match,[Int, Decimal = [_,_,_]]} ->
					list_to_integer(Int)*1000 + list_to_integer(Decimal); 
				{match,[Int, Decimal = [_,_]]} ->
					list_to_integer(Int)*1000 + list_to_integer(Decimal)*10; 
				{match,[Int, Decimal = [_]]} ->
					list_to_integer(Int)*1000 + list_to_integer(Decimal)*100; 						
				{match,[Int]} ->
					list_to_integer(Int)*1000
			end
	end.
