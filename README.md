Carbon
======

Implementations of data structures and algorithms in various languages for educational purposes.

### Setup
##### Ruby
- Install RubyGems
- Install Rake (`gem install rake`)

##### Haskell
- Run [`cabal update`][7] (this should be included in the [Haskell Platform][6])
- Run `cabal install --only-dependencies`

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
- `rebar compile && rebar escriptize && ./carbon_erlang` (benchmarks)

### What I'm working on
Statuses: 0 todo. 1 in progress. 2 implemented but could use improvements, cleanup, or minor additions. 3 stable.

##### Self balancing binary search tree (2)
- Implementation of a multiset with a self balancing binary tree
- Balanced with rotations only when needed
- TODO: benchmarking, remaining tests

##### Matrices and related math (1)
- Matrices model with related mathematical operations/concepts (e.g. vectors, matrix multiplication, row reduction)

##### Binary heap (2)
- Implementation of a binary max heap
- insertion only traverses (one path of the) tree once (`log(n)`)
- removal only travels (one path of the) tree twice (necessary to retrieve the latest node and bubble down) (`2logn`)
- TODO: generalize max/min heap, comparison function?

##### Graph (adjacency list) (1)

##### Priority queue (2)
- Backed by binary heap

##### Merge sort, quicksort, selection sort, insertion sort (2)
- TODO: benchmarking, documentation

##### Red black tree (0)

##### Fibonacci heap (0)

##### Introsort, timsort, heapsort, smoothsort (1)
- introsort (done)
- heapsort (with pairing heap)

##### Merkle tree (0)

##### Left leaning red black tree (1)
Ordered set backed by a [left leaning red black tree][10]

##### AA tree (0)

##### Todo
- sorting identify stable/unstable, time complexity, depth, etc.
- erlang benchmarks

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
[10]: http://www.cs.princeton.edu/~rs/talks/LLRB/LLRB.pdf