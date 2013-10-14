-module(carbon_erlang_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    carbon_erlang_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
	ok = application:start(carbon_erlang),
	?assertNot(undefined == whereis(carbon_erlang_sup)).
-endif.
