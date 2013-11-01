Carbon
======

Implementations of data structures and algorithms in various languages for educational purposes.

### Setup
##### Ruby
- Install RubyGems
- Install Rake (`gem install rake`)

##### Haskell
- Run [`cabal-update`][7] (this should be included in the [Haskell Platform][6])
- TODO: quick way to download dependencies?

##### Erlang
- Install [rebar][8]

### Testing
##### Ruby
- `rake test`

##### Haskell
- `cabal configure --enable-tests && cabal build && cabal test`
- `cabal configure --enable-benchmarks && cabal build && cabal bench`

##### Erlang
- `rebar compile eunit`

### What I'm working on
Statuses: 0 todo. 1 in progress. 2 implemented but could use improvements, cleanup, or minor additions. 3 stable.

##### Self balancing binary search tree (2)
- Implementation of a multiset with a self balancing binary tree
- Balanced with rotations only when neededs
- TODO: benchmarking, remaining tests, active removal, to list, from list

##### Matrices and related math (1)
- Matrices model with related mathematical operations/concepts (e.g. vectors, matrix multiplication, row reduction)

##### Binary heap (1)
- Implementation of a binary max heap
- Bubble-down insertion to avoid extra tree traversal (instead of traversing to the bottom, then bubbling up)
- TODO: generalize max/min heap, comparison function?

##### Graph (adjacency list) (1)

##### Priority queue (1)

##### Merge sort, quicksort, selection sort, insertion sort (1)

##### Red black tree (0)

##### Introsort, timsort, heapsort, smoothsort (0)

##### Todo
- sorting identify stable/unstable, time complexity, depth, etc.
- erlang benchmarks
- rename new_ with prime notation

### Goals
- use more functional paradigm (e.g. fold instead of loop)

### Challenges
- avoiding extra tree traversals in a functional language
- writing a generic print binary tree function (with a single traversal)

### Resources
- [List of data structures][1]
- [List of sorting algorithms][2]
- [Real World Haskell (book)][3]
- [Haskell Cabal Testing][4]
- [Snap Framework][5]
- [Bytestring][9]

[1]: http://en.wikipedia.org/wiki/List_of_data_structures
[2]: http://en.wikipedia.org/wiki/List_of_algorithms#Sequence_Sorting
[3]: http://book.realworldhaskell.org/
[4]: http://www.haskell.org/cabal/users-guide/developing-packages.html
[5]: https://github.com/snapframework/snap-core
[6]: http://www.haskell.org/platform/linux.html
[7]: http://www.haskell.org/haskellwiki/Cabal-Install
[8]: https://github.com/basho/rebar
[9]: https://github.com/ghc/packages-bytestring