%%%----------------------------------------------------------------------
%%% File    : redis_data.erl
%%% Author  : <vrandkode@gmail.com>
%%% Purpose : Redis data types, handling data functions.
%%% Created : 7 Jan 2015
%%%
-module(redis_data).
-export([handle_data/1,binary_to_int/1]).

-define(VOID,"null").
-define(OK,"ok").
-define(ERROR,"error").

-define(ELVIS(Cond,E1,E2), (case (Cond) of true -> (E1); false -> (E2) end)).
-define(BULK_WITH_TYPES, false).

binary_to_int(Bin) -> case string:to_integer(binary_to_list(Bin)) of {N,_} -> N end.

handle_data(Data) -> 
	handle_data(Data, {unknown, <<>>}).

handle_data(<<":",In/binary>>, {unknown, <<Out/binary>>}) ->
	data(In, {integer, <<Out/binary>>});

handle_data(<<"$",In/binary>>, {unknown, <<Out/binary>>}) ->
	data(In, {string, <<Out/binary>>});

handle_data(<<"*",In/binary>>, {unknown, <<Out/binary>>}) ->
	data(In, {array, <<Out/binary>>});	

handle_data(In, {Type, Out}) ->
	data(In, {Type,Out}).

data(<<"\r\n",In/binary>>, {string, <<Size/binary>>}) ->
	case Len = binary_to_int(Size) of 
		L when L<0 -> 
			{ok, {novalue}};
		_ -> 
			{ok, {string, <<In:Len/binary>>}}
	end;

data(<<"\r\n", In/binary>>, {array, <<Size/binary>>}) ->
	case binary_to_int(Size) of 
		L when L=<0 -> 
			{ok, {array,[]}};
		L ->
			{ok, {array, handle_data_bulk(In, L)}}
	end;

data(<<"\r\n">>,{Type, <<Out/binary>>}) ->
	{ok,{Type, Out}};

data(<<H:1/binary,Next/binary>>, {Type, <<Out/binary>>}) ->
	data(Next, {Type, <<Out/binary,H:1/binary>>}).

handle_data_bulk(<<>>, _) -> 
	[];

handle_data_bulk(<<"\r\n",In/binary>>, N) ->
	handle_data_bulk(In,N);

handle_data_bulk(<<In/binary>>, N) ->
	case handle_data(In) of 
		{_, Data = {_, <<Bin/binary>>}} when N>0 ->
			BLen = byte_size(Bin),
			PLen = string:len(integer_to_list(BLen)) + 3,
			case In of 
				<<_:PLen/binary,_:BLen/binary,Rest/binary>> ->
					[?ELVIS(?BULK_WITH_TYPES,Data,Bin)|handle_data_bulk(Rest,N-1)]; %note: replace Bin-> Bin to get bulk data with types.
				_ ->
					[?ELVIS(?BULK_WITH_TYPES,Data,Bin)]
			end;
		{_, Data = {_, <<_/binary>>}} ->
			Data;
		R -> 
			io:format("bulk ~p",[R]),
			[]
	end.
