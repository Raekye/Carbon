module Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks (benchmarks, setup) where

import qualified Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree as Tree
import Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Scaffolding
import Carbon.Benchmarking

import Control.DeepSeq

instance NFData (Tree.Tree a)

setup = force [get_tree max_size]

benchmarks = []
	++ (benchmark_function_with_values "insert" insert_tree (map (\ x -> 2 ^ x) [4..16]))
	++ (benchmark_function_with_values "search" search_tree (map (\ x -> 2 ^ x) [4..16]))
	++ (benchmark_function_with_values "remove" remove_tree (map (\ x -> 2 ^ x) [4..16]))