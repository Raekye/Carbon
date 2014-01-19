module Carbon.DataStructures.Graphs.AdjacencyList.Tests (tests) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Data.List (foldl')

import qualified Carbon.DataStructures.Graphs.AdjacencyList as AdjacencyList

tests :: [Test]
tests = [
		testCase "hi" testc
		, testCase "hi2" testa
		, testCase "hi3" add_nodes
	]

add_nodes :: HUnit.Assertion
add_nodes
	= let
		(AdjacencyList.AdjacencyList nodes) = foldl' (\ aggregate x -> AdjacencyList.add_node aggregate x) AdjacencyList.create [1..3]
	in HUnit.assertEqual "AdjacencyList add node" (map (\ x -> AdjacencyList.Node x []) [1..3]) nodes

stub :: a -> Maybe Int
stub _ = Nothing

testc = 1 HUnit.@?= 1

testa = do
	a <- return (return (5))
	b <- a
	HUnit.assertEqual "hi" Nothing (stub 3)