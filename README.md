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
- In the `haskell` directory, run `cabal install`. This will download dependencies such as HUnit

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

##### Self balancing binary tree (2)
- Implementation of a multiset with a self balancing binary tree

##### Matrices and related math (1)
- hmmmmm

##### Heap (0)

##### Graph (0)

##### Priority queue (0)

### Todo
- self balancing binary tree
- graph
- heap
- tests
- matrices

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