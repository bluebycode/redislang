-module(redis_json).
-export([test/1,extract/2, replace/3]).

-record(stack,{values}).

binary_to_int(Bin) -> case string:to_integer(binary_to_list(Bin)) of {N,_} -> N end.

replace(<<Field/binary>>, <<Value/binary>>, <<Pattern/binary>>) ->
	replace(Field, Pattern, {Field, <<>>},{Value, <<>>}, <<>>).

% field found it! replace with value and skip until end of old value
replace(<<>>, Pattern = <<"\"", _/binary>>, {Field, Field}, {Value, _}, Acc)->
	skip_field(Pattern, <<Acc/binary,"\"">>, Value);

replace(<<>>, Pattern, {Field, Field}, {Value,Value}, Acc)->
	<<Acc/binary,Pattern/binary>>;

%not found yet - first match of field
replace(<<F:1/binary, Rest/binary>>, <<F:1/binary,Tail/binary>>, {Field,S}, {Value,V}, Acc) ->
	replace(Rest,Tail, {Field,<<S/binary,F/binary>>},{Value,V}, <<Acc/binary,F/binary>>);

%not found yet - did not match yet
replace(<<_:1/binary, _/binary>>, <<H:1/binary,Tail/binary>>, {Field,<<>>}, {Value,V}, Acc) ->
	replace(Field,Tail, {Field,<<>>},{Value,V}, <<Acc/binary,H/binary>>);

%not found yet - not longer matching
replace(<<_:1/binary, _/binary>>, <<H:1/binary,Tail/binary>>, {Field,_}, {Value,V}, Acc) ->
	replace(Field,Tail, {Field,<<>>},{Value,V}, <<Acc/binary,H/binary>>);

%not matching - returning original
replace(Field, <<>>, {Field, _}, {_, _}, Acc)->
	Acc.

% skipping until end of value of field
skip_field(<<"\":[", Bin/binary>>, <<Acc/binary>>, <<"[", RestValue/binary>>) ->
	move_value(array, Bin, <<Acc/binary,":[">>, RestValue);

skip_field(<<"\":[", _/binary>>, <<_/binary>>, <<_/binary>>) ->
	{error, {badargument, "is not array"}};

skip_field(<<"\":\"", Value/binary>>, <<Acc/binary>>, <<NewValue/binary>>) ->
	skip_string(Value, <<Acc/binary,":\"",NewValue/binary>>);

skip_field(<<"\":{", Bin/binary>>, <<Acc/binary>>, <<"{", RestValue/binary>>) ->
	move_value(object, Bin, <<Acc/binary,":{">>, RestValue);

skip_field(<<"\":{", _/binary>>, <<_/binary>>, <<_/binary>>) ->
	{error, {badargument, "is not a object"}};

skip_field(<<"\":", Bin/binary>>, <<Acc/binary>>, <<NewValue/binary>>) ->
	move_value(number, Bin, <<Acc/binary,":">>, NewValue).

move_value(object, Bin, <<Acc/binary>>, <<"}">>) ->
	skip_object(Bin,  #stack{values=["{"]}, <<Acc/binary, "}">>);

move_value(array, Bin, <<Acc/binary>>, <<"]">>) ->
	skip_array(Bin,  #stack{values=["["]}, <<Acc/binary, "]">>);

move_value(number, Bin, <<Acc/binary>>, <<H:1/binary, RestValue/binary>>) 
		when H >=<<"0">>, H =< <<"9">> ->
	move_value(number, Bin, <<Acc/binary,H:1/binary>>, RestValue);

move_value(number, _, <<_/binary>>, <<_:1/binary, _/binary>>) ->
	{error, {badargument, "is not a number"}};

move_value(Type, Bin, <<Acc/binary>>, <<H:1/binary, RestValue/binary>>)->
	move_value(Type, Bin, <<Acc/binary, H:1/binary>>, RestValue);

move_value(number, Bin, <<Acc/binary>>, <<>>)->
	skip_number(Bin, Acc);

move_value(array, _, <<_/binary>>, <<>>)->
	{error, {badargument, "is not an array"}};

move_value(object, _, <<_/binary>>, <<>>)->
	{error, {badargument, "is not a object"}}.

%% Skipping strings
skip_string(Rest = <<"\"",_/binary>>, Acc)->
	<<Acc/binary, Rest/binary>>;

skip_string(<<_:1/binary,Rest/binary>>, Acc)->
	skip_string(Rest, Acc);

skip_string(<<>>,_)->
	{error, "malformed json"}.

%% Skipping numbers
skip_number(Rest = <<",",_/binary>>, Acc)->
	<<Acc/binary, Rest/binary>>;

skip_number(<<_:1/binary,Rest/binary>>, Acc)->
	skip_number(Rest, Acc);

skip_number(<<>>,_)->
	{error, "malformed json"}.

%% Skipping arrays
skip_array(<<"]", _/binary>>, #stack{values=[]}, _) ->
	{error, "malformed json"};

skip_array(<<"]", Rest/binary>>, #stack{values=["["|Symbols]},  Acc) ->
	skip_array(Rest, #stack{values=Symbols}, Acc);

skip_array(<<"]", _/binary>>, _,  _) ->
	{error, "malformed json"};

skip_array(<<Bin/binary>>, #stack{values=[]},  Acc) ->
	<<Acc/binary, Bin/binary>>;

skip_array(<<>>, #stack{values=[]},  Acc) ->
	Acc;

skip_array(<<"[", Rest/binary>>, #stack{values=Symbols},  Acc) ->
	skip_array(Rest, #stack{values=["["|Symbols]}, Acc);

skip_array(<<_:1/binary,Rest/binary>>, S, Acc) ->
	skip_array(Rest, S, Acc).

%% skip objects
skip_object(<<"}", _/binary>>, #stack{values=[]}, _) ->
	{object, {error, "malformed json"}};

skip_object(<<"}", Rest/binary>>, #stack{values=["{"|Symbols]}, Acc) ->
	skip_object(Rest, #stack{values=Symbols},  Acc);

skip_object(<<>>, #stack{values=[]}, Acc) ->
	Acc;

skip_object(<<Bin/binary>>, #stack{values=[]}, Acc) ->
	<<Acc/binary, Bin/binary>>;

skip_object(<<"}", _/binary>>, _, _) ->
	{object, {error, "malformed json"}};

skip_object(<<"{", Rest/binary>>, #stack{values=Symbols}, Acc) ->
	skip_object(Rest, #stack{values=["{"|Symbols]},  Acc);

skip_object(<<_:1/binary,Rest/binary>>, S, Acc) ->
	skip_object(Rest, S,  Acc).

%%
extract(<<Field/binary>>, <<Pattern/binary>>) ->
	extract(Field, Pattern, {Field, <<>>});

extract([Field], [Pattern]) ->
	extract(binary:list_to_bin(Field), binary:list_to_bin(Pattern), {Field, <<>>}).

extract(<<>>,Pattern = <<"\"", _/binary>>, {Field, Field})->
	field_extracted(Pattern, {unknown, <<>>});

extract(<<>>, Pattern, {Field, Field})->
	extract(Field, Pattern, {Field, <<>>});

extract(<<F:1/binary, Rest/binary>>, <<F:1/binary,Tail/binary>>, {Field,S}) ->
	extract(Rest, Tail, {Field,<<S/binary,F/binary>>});

extract(<<_:1/binary, _/binary>>, <<_:1/binary,Tail/binary>>, {Field,<<>>}) ->
	extract(Field, Tail, {Field,<<>>});

extract(<<_:1/binary, _/binary>>, <<_:1/binary,Tail/binary>>, {Field,_}) ->
	extract(Field, Tail, {Field,<<>>});

extract(Field, <<>>, {Field,_}) ->
	{unknown, <<>>}.

field_extracted(<<"\":[", Value/binary>>, {unknown, <<>>}) ->
	extract_from_array(Value,#stack{values=["["]},{array, <<"[">>});

field_extracted(<<"\":\"", Value/binary>>, {unknown, <<>>}) ->
	extract_from_type(Value, {string, <<>>});

field_extracted(<<"\":{", Value/binary>>, {unknown, <<>>}) ->
	extract_from_object(Value, #stack{values=["{"]},{object, <<"{">>});

field_extracted(<<"\":", Value/binary>>, {unknown, <<>>}) ->
	extract_from_type(Value, {number, <<>>});

field_extracted(<<_:1/binary, Value/binary>>, {unknown, <<>>}) ->
	field_extracted(Value, {unknown, <<>>}).

extract_from_type(<<"\"",_/binary>>, {string, V})->
	{string, binary_to_list(V)};

extract_from_type(<<",",_/binary>>, {number, V})->
	{number, binary_to_int(V)};

extract_from_type(<<H:1/binary,Rest/binary>>, {Type, V})->
	extract_from_type(Rest, {Type, <<V/binary,H/binary>>}).

%
% Handling arrays.
%
extract_from_array(<<"]", _/binary>>, #stack{values=[]}, {array, _}) ->
	{array, {error, "malformed json"}};

extract_from_array(<<"]", Rest/binary>>, #stack{values=["["|Symbols]}, {array, V}) ->
	extract_from_array(Rest, #stack{values=Symbols}, {array, <<V/binary,"]">>});

extract_from_array(<<"]", _/binary>>, _, {array, _}) ->
	{array, {error, "malformed json"}};

extract_from_array(<<_/binary>>, #stack{values=[]}, {array, V}) ->
	{array, V};

extract_from_array(<<>>, #stack{values=[]}, {array, V}) ->
	{array, V};

extract_from_array(<<"[", Rest/binary>>, #stack{values=Symbols}, {array, V}) ->
	extract_from_array(Rest, #stack{values=["["|Symbols]}, {array, <<V/binary,"[">>});

extract_from_array(<<H:1/binary,Rest/binary>>, S, {array, V}) ->
	extract_from_array(Rest, S, {array, <<V/binary,H/binary>>}).

%
% Handling objects/hashmaps.
%
extract_from_object(<<"}", B/binary>>, #stack{values=[]}, {object, _}) ->
	io:format("malformed ~p~n",[B]),
	{object, {error, "malformed json"}};

extract_from_object(<<"}", Rest/binary>>, #stack{values=["{"|Symbols]}, {object, V}) ->
	extract_from_object(Rest, #stack{values=Symbols}, {object, <<V/binary,"}">>});

extract_from_object(<<>>, #stack{values=[]}, {objects, V}) ->
	{object, V};

extract_from_object(<<_/binary>>, #stack{values=[]}, {object, V}) ->
	{object, V};

extract_from_object(<<"}", _/binary>>, _, {object, _}) ->
	{object, {error, "malformed json"}};

extract_from_object(<<"{", Rest/binary>>, #stack{values=Symbols}, {object, V}) ->
	extract_from_object(Rest, #stack{values=["{"|Symbols]}, {object, <<V/binary,"{">>});

extract_from_object(<<H:1/binary,Rest/binary>>, S, {object, V}) ->
	extract_from_object(Rest, S, {object, <<V/binary,H/binary>>}).

test(Field)->
	T = "{\"userId\":2,\"obj\":{\"field\":\"value\"},\"attackLogs\":[{\"unitWeapons\":[{\"left\":3}],\"other\":2}],\"another\":\"test\"}",
	io:format("~p~n", [T]),
	extract(binary:list_to_bin(Field), binary:list_to_bin(T)).

