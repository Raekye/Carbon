module Main (main) where

import qualified Criterion.Main as Criterion
import Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks
import Carbon.DataStructures.Trees.BinaryHeap.Benchmarks

main = Criterion.defaultMain [
	Criterion.bgroup "Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree" Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks.benchmarks
	, Criterion.bgroup "Carbon.DataStructures.Trees.BinaryHeap" Carbon.DataStructures.Trees.BinaryHeap.Benchmarks.benchmarks
	]