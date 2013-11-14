-module(carbon_algorithms_sorting).

-export([mergesort/2, quicksort/2, insertionsort/2, selectionsort/2, introsort/2, timsort/2, heapsort/2, smoothsort/2, shuffle_list/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

log2(X) ->
	math:log(X) / math:log(2).

mergesort([], _) -> [];
mergesort([X], _) -> [X];
mergesort(List, Compare) ->
	{Left, Right} = mergesort_partition(List, true, {[], []}),
	mergesort_unpartition(mergesort(Left, Compare), mergesort(Right, Compare), Compare).

mergesort_partition([], _, Aggregate) -> Aggregate;
mergesort_partition([Car | Cdr], IsOdd, {Left, Right}) ->
	{NewLeft, NewRight} =
		if IsOdd -> {[Car | Left], Right};
			true -> {Left, [Car | Right]}
		end,
	mergesort_partition(Cdr, not IsOdd, {NewLeft, NewRight}).

mergesort_unpartition([], Right, _) -> Right;
mergesort_unpartition(Left, [], _) -> Left;
mergesort_unpartition([LeftCar | LeftCdr], [RightCar | RightCdr], Compare) ->
	Side = Compare(LeftCar, RightCar),
	{NewCar, NewLeft, NewRight} =
		if Side -> {LeftCar, LeftCdr, [RightCar | RightCdr]};
			true -> {RightCar, [LeftCar | LeftCdr], RightCdr}
		end,
	[NewCar | mergesort_unpartition(NewLeft, NewRight, Compare)].

quicksort([], _) -> [];
quicksort([X], _) -> [X];
quicksort([Car | Cdr], Compare) ->
	{Left, Right} = quicksort_partition(Cdr, fun(X) -> X < Car end, {[], []}),
	quicksort(Left, Compare) ++ [Car | quicksort(Right, Compare)].

quicksort_partition([], _, Aggregate) -> Aggregate;
quicksort_partition([Car | Cdr], Test, {Left, Right}) ->
	Passed = Test(Car),
	{NewLeft, NewRight} =
		if Passed -> {[Car | Left], Right};
			true -> {Left, [Car | Right]}
		end,
	quicksort_partition(Cdr, Test, {NewLeft, NewRight}).

insertionsort([], _) -> [];
insertionsort([Car | Cdr], Compare) ->
	Helper =
		fun(Helper, Sorted, [First | Rest]) ->
			Helper(Helper, insert_into_sorted_list(First, Sorted, Compare), Rest);
		(_, Sorted, []) ->
			Sorted
		end,
	Helper(Helper, [Car], Cdr).

insert_into_sorted_list(Val, [], _) -> [Val];
insert_into_sorted_list(Val, [Car | Cdr], Compare) ->
	OnLeft = Compare(Val, Car),
	if OnLeft -> [Val | [Car | Cdr]];
		true -> [Car | insert_into_sorted_list(Val, Cdr, Compare)]
	end.

selectionsort([], _) -> [];
selectionsort([X], _) -> [X];
selectionsort([Car | Cdr], Compare) ->
	{First, _, Rest} = selectionsort_separate([Car | Cdr], Compare, Car, 0, 0),
	[First | selectionsort(Rest, Compare)].

selectionsort_separate([], _, First, FirstIndex, _) -> {First, FirstIndex, []};
selectionsort_separate([Car | Cdr], Compare, First, FirstIndex, I) ->
	Ahead = Compare(Car, First),
	{NewFirst, NewFirstIndex} =
		if Ahead -> {Car, I};
			true -> {First, FirstIndex}
		end,
	{FinalFirst, FinalFirstIndex, Rest} = selectionsort_separate(Cdr, Compare, NewFirst, NewFirstIndex, I + 1),
	{FinalFirst, FinalFirstIndex, if I == FinalFirstIndex -> Rest; true -> [Car | Rest] end}.

introsort([], _) -> [];
introsort(List, Compare) ->
	Helper =
		fun(Helper, [Car | Cdr], Depth, Max) ->
			if Depth > Max -> heapsort([Car | Cdr], Compare);
				true ->
					{Left, Right} = quicksort_partition(Cdr, fun(X) -> X < Car end, {[], []}),
					Helper(Helper, Left, Depth + 1, Max) ++ [Car | Helper(Helper, Right, Depth + 1, Max)]
			end;
		(_, [], _, _) -> []
		end,
	Helper(Helper, List, 0, trunc(log2(length(List)))).

timsort(List, Compare) -> List.

% This uses a pairing heap as I already implemented a binary heap in Haskell
heapsort(List, Compare) ->
	FromList = fun(L) ->
		lists:foldl(fun(X, A) -> carbon_datastructures:heap_insert(A, X, Compare) end, carbon_datastructures:heap_create(), L)
	end,
	ToList = fun(_, undefined) ->
		[];
	(ToList, H) ->
		{E, HP} = carbon_datastructures:heap_remove(H, Compare),
		[E | ToList(ToList, HP)]
	end,
	ToList(ToList, FromList(List)).

smoothsort(List, Compare) -> List.

shuffle_list(List) -> [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- List])].

-ifdef(TEST).

-define(LIST_SIZE, trunc(math:pow(2, 8))).

mergesort_test() ->
	SequentialList = lists:seq(1, ?LIST_SIZE),
	ShuffledList = shuffle_list(SequentialList),
	?assertEqual(SequentialList, mergesort(ShuffledList, fun(A, B) -> A < B end)).

quicksort_test() ->
	SequentialList = lists:seq(1, ?LIST_SIZE),
	ShuffledList = shuffle_list(SequentialList),
	?assertEqual(SequentialList, quicksort(ShuffledList, fun(A, B) -> A < B end)).

insertionsort_test() ->
	SequentialList = lists:seq(1, ?LIST_SIZE),
	ShuffledList = shuffle_list(SequentialList),
	?assertEqual(SequentialList, insertionsort(ShuffledList, fun(A, B) -> A < B end)).

selectionsort_test() ->
	SequentialList = lists:seq(1, ?LIST_SIZE),
	ShuffledList = shuffle_list(SequentialList),
	?assertEqual(SequentialList, selectionsort(ShuffledList, fun(A, B) -> A < B end)).

introsort_test() ->
	SequentialList = lists:seq(1, ?LIST_SIZE),
	ShuffledList = shuffle_list(SequentialList),
	?assertEqual(SequentialList, introsort(ShuffledList, fun(A, B) -> A < B end)).

heapsort_test() ->
	SequentialList = lists:seq(1, ?LIST_SIZE),
	ShuffledList = shuffle_list(SequentialList),
	?assertEqual(SequentialList, heapsort(ShuffledList, fun(A, B) -> A < B end)).

-endif.
