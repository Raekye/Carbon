module Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Tests (tests, get_tree, insert_tree, search_tree, remove_tree, max_size) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree as Tree
import Carbon.Testing
import qualified Carbon.DataStructures.Trees.NaturalTree as NaturalTree

max_size :: Integer
max_size = 2 ^ 16

get_tree :: Integer -> Tree.Tree Integer
get_tree
	= let
		get_tree' 0 = Tree.create
		get_tree' n = Tree.add (get_tree (n - 1)) (distribute_range n)
	in NaturalTree.index (fmap get_tree' NaturalTree.naturals)

insert_tree :: Integer -> Tree.Tree Integer
insert_tree n
	= if n > 0 then (Tree.add (get_tree (n - 1)) (distribute_range n)) else get_tree 0

search_tree :: Integer -> Int
search_tree n = Tree.count (get_tree n) n

remove_tree :: Integer -> Tree.Tree Integer
remove_tree n
	= if n > 0 then (Tree.removeall (get_tree (n - 1)) (distribute_range n)) else get_tree 0

tests :: [Test]
tests = [ test_a
	, prop_height
	, prop_valid
	]

test_a :: Test
test_a = testCase "SelfBalancingBinarySearchTree/test_a" $ HUnit.assertEqual "msg" "foo" "foo"

prop_height ::Test
prop_height = testProperty "SelfBalancingBinarySearchTree/prop_height" $ prop_height'

prop_height' :: Integer -> Property
prop_height' n = (n > 0 && n < max_size) ==> (Tree.height (get_tree n)) == (truncate (logBase 2 (fromIntegral n)))

prop_valid :: Test
prop_valid = testProperty "SelfBalancingBinarySearchTree/prop_valid" $ prop_valid'

prop_valid' :: Integer -> Property
prop_valid' n = (n > 0 && n < max_size) ==> validate_tree $ get_tree n

-- insert and remove sequential, insert and remove distributed

--prop_e :: Test
--prop_e = testProperty "SelfBalancingBinarySearchTree/prop_e" $ prop_e'

--prop_e' :: Integer -> Property
--prop_e' n = (n < max_size) ==> foldl' (\ )

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
