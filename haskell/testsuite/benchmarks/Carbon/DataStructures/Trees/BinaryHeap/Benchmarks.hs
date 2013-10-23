module Carbon.DataStructures.Trees.BinaryHeap.Benchmarks (benchmarks) where

import qualified Criterion.Main as Criterion
import qualified Carbon.DataStructures.Trees.BinaryHeap as Tree
import Data.List
import Debug.Trace
import Data.List

distributed_range :: Int -> [Int]
distributed_range n = map (\ x -> (truncate (sin (fromIntegral (x ^ 2)) * (fromIntegral n)))) [1..n]

create_tree :: Int -> Tree.Tree Int
create_tree n = foldl' (\ tree val -> (Tree.insert tree val)) Tree.create $ distributed_range n

benchmark_function_with_values name fn values = map (\ x -> Criterion.bench (name ++ "-" ++ (show x)) (Criterion.whnf fn x)) values

benchmarks = []
	++ (benchmark_function_with_values "insert" create_tree (map (\ x -> 2 ^ x) [4..16]))