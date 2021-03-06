-module(carbon_benchmarking).
-export([eprof/3, timer/3]).

eprof(F, Args, Iterations) ->
	eprof:start_profiling([self()]),
	lists:foreach(fun(_) -> erlang:apply(F, Args) end, lists:seq(1, Iterations)),
	eprof:stop_profiling(),
	eprof:analyze(total).

timer(F, Args, Iterations) ->
	IterationsSeq = lists:seq(1, Iterations),
	TimeOnce = fun() ->
		{Micros, _} = timer:tc(F, Args),
		Micros
	end,
	AllTimes = lists:map(fun(_) -> TimeOnce() end, IterationsSeq),
	Min = lists:min(AllTimes),
	Max = lists:max(AllTimes),
	Med = lists:nth(round(Iterations / 2), lists:sort(AllTimes)),
	Avg = lists:foldl(fun(Elem, Accumulator) -> Accumulator + Elem end, 0, AllTimes) / Iterations,
	{TotalByGroupedIterations, _} = timer:tc(fun() -> lists:foreach(fun(_) -> erlang:apply(F, Args) end, IterationsSeq) end),
	AvgByGroupedIterations = TotalByGroupedIterations / Iterations,
	{Min, Max, Med, Avg, AvgByGroupedIterations}.