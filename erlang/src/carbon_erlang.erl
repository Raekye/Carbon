-module(carbon_erlang).

-export([main/1]).

main(Args) ->
	eprof:start(),
	lists:map(fun({Name, F, Args, Iterations}) ->
		carbon_benchmarking:eprof(F, Args, Iterations),
		io:format("~s: ~p~n", [Name, carbon_benchmarking:timer(F, Args, Iterations)])
	end, carbon_algorithms_benchmarks:benchmarks()),
	eprof:stop().