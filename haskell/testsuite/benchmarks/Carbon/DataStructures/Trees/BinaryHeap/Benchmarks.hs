module Carbon.DataStructures.Trees.BinaryHeap.Benchmarks (benchmarks, setup) where

import qualified Criterion.Main as Criterion
import qualified Carbon.DataStructures.Trees.BinaryHeap as Tree
import qualified Carbon.DataStructures.Trees.NaturalTree as NaturalTree
import Data.List
import Debug.Trace
import Data.List
import Control.DeepSeq

instance NFData (Tree.Tree a)

distributed_range :: Integer -> [Integer]
distributed_range n = map (\ x -> (truncate (sin (fromIntegral (x ^ 2)) * (fromIntegral n)))) [1..n]

create_tree :: Integer -> Tree.Tree Integer
create_tree n = Tree.from_list [1..n]

get_tree :: Integer -> Tree.Tree Integer
get_tree
	= let
		get_tree' 0 = create_tree 0
		get_tree' n = Tree.insert (get_tree (n - 1)) n
	in NaturalTree.index (fmap get_tree' NaturalTree.naturals)

insert_tree :: Integer -> Tree.Tree Integer
insert_tree n
	= if n > 0 then (Tree.insert (get_tree (n - 1)) n) else get_tree 0

remove_tree :: Integer -> Tree.Tree Integer
remove_tree n
	= if n > 0 then (fst (Tree.remove (get_tree (n - 1)))) else get_tree 0

setup = force [get_tree (2 ^ 16)]

benchmark_function_with_values name fn values = map (\ x -> Criterion.bench (name ++ "-" ++ (show x)) (Criterion.whnf fn x)) values

benchmarks = []
--	++ (benchmark_function_with_values "_loading_tree" get_tree [2 ^ 16])
	++ (benchmark_function_with_values "insert" insert_tree (map (\ x -> 2 ^ x) [4..16]))
	++ (benchmark_function_with_values "remove" remove_tree (map (\ x -> 2 ^ x) [4..16]))