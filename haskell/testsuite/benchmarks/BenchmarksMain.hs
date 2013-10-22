module Main (main) where

import qualified Criterion.Main as Criterion
import Carbon.DataStructures.Trees.SelfBalancingBinaryTree.Benchmarks

main = Criterion.defaultMain [
	Criterion.bgroup "Carbon.DataStructures.Trees.SelfBalancingBinaryTree" Carbon.DataStructures.Trees.SelfBalancingBinaryTree.Benchmarks.benchmarks
	]