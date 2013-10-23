module Main (main) where

import qualified Criterion.Main as Criterion
import Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks

main = Criterion.defaultMain [
	Criterion.bgroup "Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree" Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks.benchmarks
	]