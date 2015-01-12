%%%----------------------------------------------------------------------
%%% File    : redis_app.erl
%%% Author  : <vrandkode@gmail.com>
%%% Purpose : Redis socket client - Application
%%% Created : 12 Jan 2015
%%%
-module(redis_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    redis_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================