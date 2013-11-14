-module(carbon_datastructures).
-export([heap_create/0, heap_insert/3, heap_remove/2]).

% Data structures used in Erlang algorithms
% Other data structures in the Carbon project in Haskell

% Pairing heap
-record(heap, {elem, subheaps}).

heap_create() -> undefined.

heap_insert(H, E, F) -> heap_merge(H, #heap{elem=E, subheaps=[]}, F).

heap_remove(#heap{elem=E, subheaps=[Car | Cdr]}, F) ->
	{E, lists:foldl(fun(X, A) -> heap_merge(A, X, F) end, Car, Cdr)};
heap_remove(#heap{elem=E, subheaps=[]}, _) ->
	{E, undefined}.

heap_merge(undefined, H2, _) ->
	H2;
heap_merge(H1 = #heap{elem=E1, subheaps=SH1}, H2 = #heap{elem=E2, subheaps=SH2}, F) ->
	LT = F(E1, E2),
	if LT -> #heap{elem=E1, subheaps=[H2 | SH1]};
		true -> #heap{elem=E2, subheaps=[H1 | SH2]}
	end.
