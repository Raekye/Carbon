module Carbon.DataStructures.Trees.BinaryHeap.Tests (tests) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Carbon.DataStructures.Trees.BinaryHeap as Tree
import Carbon.DataStructures.Trees.BinaryHeap.Scaffolding

import Debug.Trace

tests :: [Test]
tests = [ test_a
	, prop_height
	, prop_insert
	, prop_remove
	, prop_insert_distributed
	, prop_remove_distributed
	]

test_a :: Test
test_a = testCase "BinaryHeap/test_a" $ HUnit.assertEqual "msg" "foo" "foo"

prop_height ::Test
prop_height = testProperty "BinaryHeap/prop_height" $ \ x -> (x > 0 && x < max_size) ==> (Tree.height (get_tree x)) == (truncate . (logBase 2) . fromIntegral) x

prop_insert :: Test
prop_insert = testProperty "BinaryHeap/prop_insert" $ \ x -> (x > 0 && x < max_size) ==> validate_tree . insert_tree $ x

prop_remove :: Test
prop_remove = testProperty "BinaryHeap/prop_remove" $ \ x -> (x > 0 && x < max_size) ==> validate_tree . remove_tree $ x

prop_insert_distributed :: Test
prop_insert_distributed = testProperty "BinaryHeap/prop_insert_distributed" $ \ x -> (x > 0 && x < max_size) ==> validate_tree . get_distributed_tree $ x

prop_remove_distributed :: Test
prop_remove_distributed = testProperty "BinaryHeap/prop_remove_distributed" $ \ x -> (x > 0 && x < max_size) ==> validate_tree . fst . Tree.remove . get_distributed_tree $ x

validate_tree :: (Ord a) => Tree.Tree a -> Bool
validate_tree (Tree.Leaf _) = True
validate_tree (Tree.Branch (Tree.Leaf Tree.Max) _ (Tree.Leaf Tree.Max) size Tree.Max)
	= size == 1
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_size Tree.Max) node (Tree.Leaf Tree.Max) size Tree.Max)
	= (left_node <= node) && (size == left_size + 1) && (validate_tree (Tree.Branch left_left left_node left_right left_size Tree.Max))
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_size Tree.Max) node (Tree.Branch right_left right_node right_right right_size Tree.Max) size Tree.Max)
	= (left_node <= node) && (right_node <= node) && (size == left_size + right_size + 1) && (validate_tree (Tree.Branch left_left left_node left_right left_size Tree.Max)) && (validate_tree (Tree.Branch right_left right_node right_right right_size Tree.Max))