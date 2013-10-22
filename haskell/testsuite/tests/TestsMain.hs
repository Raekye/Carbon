module Main where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Carbon.DataStructures.Trees.SelfBalancingBinaryTree.Tests
import qualified Carbon.DataStructures.Trees.BinaryHeap.Tests

main :: IO ()
main = defaultMain [testGroup "Carbon.DataStructures.Trees.SelfBalancingBinaryTree.Tests" Carbon.DataStructures.Trees.SelfBalancingBinaryTree.Tests.tests
	, testGroup "Carbon.DataStructures.Trees.BinaryHeap.Tests" Carbon.DataStructures.Trees.BinaryHeap.Tests.tests
	, testGroup "Stub" [tests_a]
	]

tests_a :: Test
tests_a = testGroup "Test A" [
	testProperty "Test A - Property A" prop_a
	, testProperty "Test B - Property B" prop_b
	, testCase "Test C - Property C" test_c
	]

prop_a :: Int -> Bool
prop_a x = True

prop_b :: Int -> Bool
prop_b x = True

test_c = 1 HUnit.@?= 1