Carbon
======

Implementations of data structures and algorithms in various languages for educational purposes.

### Setup
##### Ruby
- Install RubyGems
- Install Rake (`gem install rake`)

##### Haskell
- Install the [Haskell Platform][6]. This should include [Cabal-Install][7]
- Run `cabal-update`
- TODO: quick way to download dependencies?

##### Erlang
- Install [rebar][8]

### Testing
##### Ruby
- `rake test`

##### Haskell
- `cabal configure --enable-tests && cabal build && cabal test`

##### Erlang
- `rebar compile eunit`

### What I'm working on
Statuses: 0 todo. 1 in progress. 2 implemented but could use improvements/cleanup. 3 stable.

##### Self balancing binary search tree (2)
- Implementation of a multiset with a self balancing binary tree
- Balanced with rotations only when neededs
- TODO: benchmarking, remaining tests

##### Matrices and related math (1)
- Matrices model with related mathematical operations/concepts (e.g. vectors, matrix multiplication, row reduction)

##### Binary Heap (1)
- Implementation of a binary max heap
- Bubble-down insertion to avoid extra tree traversal (instead of traversing to the bottom, then bubbling up)
- TODO: generalize max/min heap, comparison function?

##### Graph (Adjacency List) (1)

##### Priority queue (1)

##### Merge sort, Quicksort, Selection sort, Insertion sort (0)

##### Todo
- ci server
- add carbon namespace
- rename self balancing binary search tree

### Goals
- use more functional paradigm (e.g. fold instead of loop)

### Resources
- [List of data structures][1]
- [List of sorting algorithms][2]
- [Real World Haskell (book)][3]
- [Haskell Cabal Testing][4]
- [Snap Framework][5]

[1]: http://en.wikipedia.org/wiki/List_of_data_structures
[2]: http://en.wikipedia.org/wiki/List_of_algorithms#Sequence_Sorting
[3]: http://book.realworldhaskell.org/
[4]: http://www.haskell.org/cabal/users-guide/developing-packages.html
[5]: https://github.com/snapframework/snap-core
[6]: http://www.haskell.org/platform/linux.html
[7]: http://www.haskell.org/haskellwiki/Cabal-Install
[8]: https://github.com/basho/rebar