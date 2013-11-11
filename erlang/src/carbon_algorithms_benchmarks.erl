-module(carbon_algorithms_benchmarks).
-export([benchmarks/0]).

-define(LIST_SIZE, trunc(math:pow(2, 10))).
-define(ITERATIONS, trunc(math:pow(2, 7))).

benchmarks() ->
	ShuffledList = carbon_algorithms_sorting:shuffle_list(lists:seq(1, ?LIST_SIZE)),
	Compare = fun(A, B) -> A < B end,
	Args = [ShuffledList, Compare],
	[
		{"Mergesort", fun(A, B) -> carbon_algorithms_sorting:mergesort(A, B) end, Args, ?ITERATIONS},
		{"Quicksort", fun(A, B) -> carbon_algorithms_sorting:quicksort(A, B) end, Args, ?ITERATIONS},
		{"Insertion sort", fun(A, B) -> carbon_algorithms_sorting:insertionsort(A, B) end, Args, ?ITERATIONS},
		{"Selection sort", fun(A, B) -> carbon_algorithms_sorting:selectionsort(A, B) end, Args, ?ITERATIONS}
		{"Introsort", fun(A, B) -> carbon_algorithms_sorting:introsort(A, B) end, Args, ?ITERATIONS}
	].
