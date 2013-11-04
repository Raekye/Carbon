module Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Tests (tests) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree as Tree
import Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Scaffolding
import Carbon.Testing

import Debug.Trace

tests :: [Test]
tests = [ test_a
	, prop_height
	, prop_insert
	, prop_insert_distributed
	, prop_remove_distributed
	]

test_a :: Test
test_a = testCase "SelfBalancingBinarySearchTree/test_a" $ HUnit.assertEqual "msg" "foo" "foo"

prop_height ::Test
prop_height = testProperty "SelfBalancingBinarySearchTree/prop_height" $ \ x -> (x > 0 && x < max_size) ==> Tree.height (get_tree x) <= ((truncate . (logBase golden_ratio)) ((sqrt 5) * (fromIntegral (x + 2)))) - 2

prop_insert :: Test
prop_insert = testProperty "SelfBalancingBinarySearchTree/prop_insert" $ \ x -> (x > 0 && x < max_size) ==> validate_tree $ get_tree x

prop_remove :: Test
prop_remove = testProperty "SelfBalancingBinarySearchTree/prop_remove" $ \ x -> (x > 0 && x < max_size) ==> validate_tree $ get_tree x

prop_insert_distributed :: Test
prop_insert_distributed = testProperty "SelfBalancingBinarySearchTree/prop_insert_distributed" $ \ x -> (x > 0 && x < max_size) ==> validate_tree $ get_distributed_tree x

prop_remove_distributed :: Test
prop_remove_distributed = testProperty "SelfBalancingBinarySearchTree/prop_remove_distributed" $ \ x -> (x > 0 && x < max_size) ==> validate_tree $ Tree.remove (get_distributed_tree x) (distribute_range x)

validate_tree :: (Ord a) => Tree.Tree a -> Bool
validate_tree (Tree.Branch Tree.Leaf node Tree.Leaf n h)
	= (h == 0)
validate_tree (Tree.Branch Tree.Leaf node (Tree.Branch right_left right_node right_right right_n right_h) n h)
	= (node < right_node) && (h == right_h + 1) && validate_tree (Tree.Branch right_left right_node right_right right_n right_h)
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_n left_h) node Tree.Leaf n h)
	= (left_node < node) && (h == left_h + 1) && validate_tree (Tree.Branch left_left left_node left_right left_n left_h)
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_n left_h) node (Tree.Branch right_left right_node right_right right_n right_h) n h)
	= (left_node < node && node < right_node) && (h == (max left_h right_h) + 1) && ((validate_tree (Tree.Branch left_left left_node left_right left_n left_h)) && (validate_tree (Tree.Branch right_left right_node right_right right_n right_h)))
validate_tree (Tree.Leaf) = True
