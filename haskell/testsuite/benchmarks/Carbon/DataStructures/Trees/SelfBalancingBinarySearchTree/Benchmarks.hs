module Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks (benchmarks) where

import qualified Criterion.Main as Criterion
import qualified Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree as Tree
import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree
import qualified Carbon.DataStructures.Trees.NaturalTree as NaturalTree
import Data.List
import Debug.Trace
import Data.List

distributed_range :: Integer -> [Integer]
distributed_range n = map (\ x -> (truncate (sin (fromIntegral (x ^ 2)) * (fromIntegral n)))) [1..n]

create_tree :: Integer -> Tree.Tree Integer
create_tree n = foldl' (\ tree val -> (Tree.add tree val)) Tree.create $ distributed_range n

get_tree :: Integer -> Tree.Tree Integer
get_tree
	= let
		get_tree' 0 = create_tree 0
		get_tree' n = Tree.add (get_tree (n - 1)) n
	in NaturalTree.index (fmap get_tree' NaturalTree.naturals)

search_tree :: Integer -> Int
search_tree n = Tree.count (get_tree n) n

benchmark_function_with_values name fn values = map (\ x -> Criterion.bench (name ++ "-" ++ (show x)) (Criterion.whnf fn x)) values

benchmarks = []
	++ (benchmark_function_with_values "insert" create_tree (map (\ x -> 2 ^ x) [4..16]))
	++ (benchmark_function_with_values "get_tree" get_tree (map (\ x -> 2 ^ x) [4..16]))
	++ (benchmark_function_with_values "search" search_tree (map (\ x -> 2 ^ x) [4..16]))