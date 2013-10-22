module Carbon.DataStructures.Trees.SelfBalancingBinaryTree.Benchmarks (benchmarks) where

import qualified Criterion.Main as Criterion
import qualified Carbon.DataStructures.Trees.SelfBalancingBinaryTree as Tree
import Data.List

create_tree n = foldl' (\ tree val -> (Tree.add tree val)) Tree.create [1..n]

get_tree
	= let
		get_tree' n = create_tree n
	in (map get_tree' [0 ..] !!)

search_tree n = Tree.count (get_tree n) n

benchmarks = [
	Criterion.bench "insert 16" $ Criterion.whnf create_tree (2 ^ 4)
	, Criterion.bench "insert 32" $ Criterion.whnf create_tree (2 ^ 5)
	, Criterion.bench "insert 64" $ Criterion.whnf create_tree (2 ^ 6)
	, Criterion.bench "insert 128" $ Criterion.whnf create_tree (2 ^ 7)
	, Criterion.bench "insert 256" $ Criterion.whnf create_tree (2 ^ 8)
	, Criterion.bench "insert 512" $ Criterion.whnf create_tree (2 ^ 9)
	, Criterion.bench "insert 1024" $ Criterion.whnf create_tree (2 ^ 10)
	, Criterion.bench "insert 2048" $ Criterion.whnf create_tree (2 ^ 11)
	, Criterion.bench "insert 4096" $ Criterion.whnf create_tree (2 ^ 12)
	, Criterion.bench "insert 8192" $ Criterion.whnf create_tree (2 ^ 13)
	, Criterion.bench "insert 16384" $ Criterion.whnf create_tree (2 ^ 14)
	, Criterion.bench "insert 32768" $ Criterion.whnf create_tree (2 ^ 15)
	, Criterion.bench "insert 65536" $ Criterion.whnf create_tree (2 ^ 16)
	, Criterion.bench "search 16" $ Criterion.whnf search_tree (2 ^ 4)
	, Criterion.bench "search 32" $ Criterion.whnf search_tree (2 ^ 5)
	, Criterion.bench "search 64" $ Criterion.whnf search_tree (2 ^ 6)
	, Criterion.bench "search 128" $ Criterion.whnf search_tree (2 ^ 7)
	, Criterion.bench "search 256" $ Criterion.whnf search_tree (2 ^ 8)
	, Criterion.bench "search 512" $ Criterion.whnf search_tree (2 ^ 9)
	, Criterion.bench "search 1024" $ Criterion.whnf search_tree (2 ^ 10)
	, Criterion.bench "search 2048" $ Criterion.whnf search_tree (2 ^ 11)
	, Criterion.bench "search 4096" $ Criterion.whnf search_tree (2 ^ 12)
	, Criterion.bench "search 8192" $ Criterion.whnf search_tree (2 ^ 13)
	, Criterion.bench "search 16384" $ Criterion.whnf search_tree (2 ^ 14)
	, Criterion.bench "search 32768" $ Criterion.whnf search_tree (2 ^ 15)
	, Criterion.bench "search 65536" $ Criterion.whnf search_tree (2 ^ 16)
	]