-module(carbon_erlang).

-export([main/1]).

main(Args) ->
	eprof:start(),
	lists:map(fun({Name, F, Args, Iterations}) ->
		carbon_benchmarking:eprof(F, Args, Iterations),
		{Min, Max, Med, Avg, AvgByGroupedIterations} = carbon_benchmarking:timer(F, Args, Iterations),
		io:format("~s: Min ~B; Max ~B; Med ~B; Avg ~f; Avg by grouped iterations: ~f.~n", [Name, Min, Max, Med, Avg, AvgByGroupedIterations])
	end, carbon_algorithms_benchmarks:benchmarks()),
	eprof:stop().