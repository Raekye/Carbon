module Carbon.DataStructures.Trees.SelfBalancingBinaryTree.Tests (tests) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Carbon.DataStructures.Trees.SelfBalancingBinaryTree as Tree
import Data.List

create_tree n = foldl' (\ tree val -> (Tree.add tree val)) Tree.create [1..n]
max_size = (truncate (2 ** 8))

tests :: [Test]
tests = [ test_a
	, prop_b
	, prop_c
	, prop_d
	]

test_a :: Test
test_a = testCase "SelfBalancingBinaryTree/test_a" $ HUnit.assertEqual "msg" "foo" "foo"

prop_b :: Test
prop_b = testProperty "SelfBalancingBinaryTree/prop_b" $ prop_b'

prop_b' :: Int -> Bool
prop_b' x = True

prop_c ::Test
prop_c = testProperty "SelfBalancingBinaryTree/prop_c" $ prop_c'

prop_c' :: Int -> Property
prop_c' n = (n > 0 && n < max_size) ==> (Tree.height (create_tree n)) == (truncate (logBase 2 (fromIntegral n)))

prop_d :: Test
prop_d = testProperty "SelfBalancingBinaryTree/prop_d" $ prop_d'

prop_d' :: Int -> Property
prop_d' n = (n < max_size) ==> validate_tree $ create_tree n

validate_tree (Tree.Branch Tree.Leaf node Tree.Leaf n h)
	= (h == 0)
validate_tree (Tree.Branch Tree.Leaf node (Tree.Branch right_left right_node right_right right_n right_h) n h)
	= (node < right_node) && (h == right_h + 1) && validate_tree (Tree.Branch right_left right_node right_right right_n right_h)
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_n left_h) node Tree.Leaf n h)
	= (left_node < node) && (h == left_h + 1) && validate_tree (Tree.Branch left_left left_node left_right left_n left_h)
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_n left_h) node (Tree.Branch right_left right_node right_right right_n right_h) n h)
	= (left_node < node && node < right_node) && (h == (max left_h right_h) + 1) && ((validate_tree (Tree.Branch left_left left_node left_right left_n left_h)) && (validate_tree (Tree.Branch right_left right_node right_right right_n right_h)))
validate_tree (Tree.Leaf) = True
