%%%----------------------------------------------------------------------
%%% File    : redis.erl
%%% Author  : Alvaro Lopez Sanchez <alvaro.lopez@omnidrone.net>
%%% Purpose : Redis socket client
%%% Created : 7 Jan 2015
%%%

-module(redis).
-author('alvaro.lopez@omnidrone.net').

-behaviour(gen_server).

-include("logger.hrl").

-export([start_link/2,start_link/3,start/2,start/3,stop/1]).

-export([hget/3]).

% call backs exports
-export([init/1, 
				handle_info/2,
				handle_cast/2,
				handle_call/3,
        terminate/2, 
        code_change/3]).

-define(CRLF, "\r\n").

-record(state, {host,port,socket,source}).

start(normal, _Args) ->
  ?DEBUG("***** redis start ~p ~n",[""]),
  ok;

start(Address, Port)->
	?DEBUG("***** redis start ~p ~n",[""]),
	gen_server:start(?MODULE, [Address, Port], []).

start(Address, Port, Options)->
	?DEBUG("***** redis start 2~p ~n",[""]),
	gen_server:start(?MODULE, [Address, Port, Options], []).

%% @doc Disconnect the socket and stop the process.
stop(Pid) ->
	gen_server:call(Pid, stop, infinity).

%% @doc Create a linked process to talk with the redis node server.
start_link(Address, Port) ->
	?DEBUG("***** redis startlink ~p ~n",[""]),
    start_link(Address, Port, []).

%% @doc Create a linked process to talk with the redis node.
start_link(Address, Port, Options) when is_list(Options) ->
		?DEBUG("***** redis startlink ~p ~n",[""]),
    gen_server:start_link(?MODULE, [Address, Port, Options], []).


%% @doc Send by tcp connection every argument of redis command recursively given the list: 
% establishing the second part of redis protocol: 
%
% ['$SizeArgument-i\r\nArgument-i\r\n' || i<-seq(1,NumberOfArguments)]
%
%% @spec (_Socket::socket(), Arg::string(), Rest::list()) ->
%%     ok | {error, Reason}
send_arguments(_Socket, []) ->ok;
send_arguments(_Socket, [Arg|Rest]) ->
	%io:format("sending arg -> '~s' | '~s'~n",[[Arg],Rest]),
 	%io:format("sending argument -> '~s~s~s~s~s'~n", ["\$",  integer_to_list(iolist_size(Arg)), ?CRLF, Arg, ?CRLF]),
  case gen_tcp:send(_Socket, ["\$",  integer_to_list(iolist_size(Arg)), ?CRLF, Arg, ?CRLF]) of
    ok -> send_arguments(_Socket, Rest);
    {error, _Error} -> {error, _Error}
  end.

%% @doc Send by tcp connection the first part redis command which includes number of arguments expected to send.
%% establishing the first part of redis protocol: 
%
%% '*NumberOfArguments\r\n'
%
%% @spec (_Socket::socket(), Arguments::list()) ->
%%     ok | {error, Reason}
send_header(_Socket, Arguments) ->
	%io:format("sending header -> '~s~s~s'~n", ["*", integer_to_list(length(Arguments)), ?CRLF]),
  gen_tcp:send(_Socket, ["*", integer_to_list(length(Arguments)), ?CRLF]).

%% @doc Send redis command under redis protocol: 
%% @spec (_Socket::socket(), Arguments::list()) ->
%%     ok | {error, Reason}
sendCommand(_Socket, Command, Args) ->
	Arguments = [Command|Args],
	?DEBUG("***** sendCommand: ~p = ~p = ~p ~n",[_Socket,Command,Args]),
	case send_header(_Socket, Arguments) of 
		ok -> send_arguments(_Socket, Arguments);
		{error, _Error} -> {error, _Error}
	end.

%% @doc if last two chars do not contains '\r\n' means packet is incomplete
%% @todo: check case when packet is corrupted to avoid socket got stuck waiting for EOF.
%% @spec (_Socket::socket(), Buffer:binary()) ->
%%     true | false
has_next(_Socket, <<>>) -> false;
has_next(_Socket, Buffer) ->
	Skip = byte_size(Buffer) - 2,
	case Buffer of
		<<_:Skip/binary, "\r\n">> -> false;
		_ -> true
	end.

%% @doc receive more data from socket connection.
%% @spec (_Socket::socket() ->
%%     binary() | <<>>
next(_Socket) ->
	case gen_tcp:recv(_Socket, 0) of
    {ok, Next} -> Next;
    _ -> <<>>
  end.

%% @doc if more tokens expected it will ask for them.
%% @spec (_Socket::socket() ->
%%     binary() | <<>>
handle_next(_Socket, Buffer) ->
	case has_next(_Socket, Buffer) of 
		false -> 
			Buffer;
		true ->
			Rest = handle_next(_Socket, next(_Socket)),
			<<Buffer/binary,Rest/binary>>
	end.

%% @doc parse the response binary into right representation {ok, {type, ParsedBinary}}.
%% @spec (Data::binary() ->
%%   {ok, {type, binary()}} | error
parse_response(Data)->
	try redis_data:handle_data(Data) of 
		_Response -> _Response
	catch
		throw:{error,Message} -> 
			io:format("error thrown: ~p~n",[Message]),
			error
	end.
	
%%--------------------------------------------------------------------
%% @doc http://redis.io/commands/hget
%% @spec hget(Server, Key, Hash) -> {ok,{string|integer|unknown, Value}
%% @end
%%--------------------------------------------------------------------
hget(Pid, Key, Hash) -> 
	?DEBUG("***** hget: ~p = ~p = ~p ~n",[Pid,Key,Hash]),
	gen_server:call(Pid, {hget, [Key,[Hash]]}).

%% @private
%% Connects if disconnected.
connect(#state{host=Host, port=Port, socket=_Socket}) ->
	?DEBUG("***** connecting...~p ~n",[""]),
	case gen_tcp:connect(Host, Port, [binary, {active, once}, {packet, raw},{reuseaddr, true}]) of
    {ok, _Socket} ->
    	?DEBUG("***** connected? ~p ~n",[_Socket]),
    	{ok, #state{socket=_Socket}};
    Error ->
    	?DEBUG("***** connected? ~p ~n",["ERROR"]),
      Error
    end.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

%% @doc callback triggered by generic server on termination
terminate(_, _)->
	ok.

%% @doc callback triggered by generic server on code change
%% No changes!! :D
code_change(PreviousVersion, State, Extra)->
	?DEBUG("***** code_change ~p ~p ~p ~n",[PreviousVersion, State, Extra]),
	{ok, State}.

%% @private
init([Address, Port, Options])->
	?DEBUG("***** connect~p ~n",[Options]),
	case connect(#state{host=Address, port=Port}) of
			{ok, State} -> {ok, State};
      {error, _Reason} -> {error, _Reason}
  end.

%% @doc callback triggered by generic server on tcp reception data (from redis protocol communication: the only one expected)
%% parsing the response and replying to him back.
%%
%% @spec ({tcp, _, Data}, State::record(#state) ->
%%   {noreply, State};
handle_info({tcp, _, Data}, State = #state{socket=_Socket,source=_From}) ->
	_Response = handle_next(_Socket, Data),
	_Reply = parse_response(_Response),
	gen_server:reply(_From, _Reply),
	{noreply, State};

handle_info(_Unknown, State)->
	{noreply, State}.

handle_cast(Message, State) ->
	?DEBUG("***** handle_cast ~p ~n",[Message]),
	{noreply,State}.

%% @doc callback triggered by generic server to set up a redis protocol communication
%% with the server and send the command.
%%
%% State:record(#state) contains client pid to provide replying to him back.
%%
%% @spec ({ Cmd, Args }, _From, State ->
%%   {noreply, State#state{source=ClientReference}}. | error
handle_call({ Cmd, Args }, _From, State = #state{socket=_Socket}) ->
	ok = sendCommand(_Socket, atom_to_list(Cmd), Args),
	{noreply, State#state{source=_From}}.

