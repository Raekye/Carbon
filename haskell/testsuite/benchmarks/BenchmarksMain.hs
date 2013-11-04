module Main (main) where

import qualified Criterion.Main as Criterion
import qualified Criterion.Config
import System.FilePath (FilePath)

import Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks
import Carbon.DataStructures.Trees.BinaryHeap.Benchmarks

import Control.Exception
import Control.DeepSeq
import Debug.Trace

criterion_config = Criterion.Config.defaultConfig { Criterion.Config.cfgReport = Criterion.Config.ljust "dist/build/bench-builder-all/carbon-benchmarks.html" }

main = do
	evaluate Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks.setup
	evaluate Carbon.DataStructures.Trees.BinaryHeap.Benchmarks.setup
	Criterion.defaultMainWith criterion_config (return ()) [
		Criterion.bgroup "Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree" Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Benchmarks.benchmarks
		, Criterion.bgroup "Carbon.DataStructures.Trees.BinaryHeap" Carbon.DataStructures.Trees.BinaryHeap.Benchmarks.benchmarks
		]