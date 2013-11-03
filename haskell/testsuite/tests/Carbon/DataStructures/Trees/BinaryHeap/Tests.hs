module Carbon.DataStructures.Trees.BinaryHeap.Tests (tests, max_size, get_tree, get_distributed_tree, insert_tree, remove_tree) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Carbon.DataStructures.Trees.BinaryHeap as Tree
import qualified Carbon.DataStructures.Trees.NaturalTree as NaturalTree
import Carbon.Testing

import Debug.Trace

max_size :: Integer
max_size = 2 ^ 16

get_tree :: Integer -> Tree.Tree Integer
get_tree
	= let
		get_tree' 0 = Tree.create Tree.Max
		get_tree' n = Tree.insert (get_tree (n - 1)) n
	in NaturalTree.index (fmap get_tree' NaturalTree.naturals)

get_distributed_tree :: Integer -> Tree.Tree Integer
get_distributed_tree
	= let
		get_tree' 0 = Tree.create Tree.Max
		get_tree' n = Tree.insert (get_tree (distribute_range n)) n
	in NaturalTree.index (fmap get_tree' NaturalTree.naturals)

insert_tree :: Integer -> Tree.Tree Integer
insert_tree n
	= Tree.insert (get_tree (n - 1)) n

remove_tree :: Integer -> Tree.Tree Integer
remove_tree n
	= fst . Tree.remove . get_tree $ n

tests :: [Test]
tests = [ test_a
	, prop_height
	, prop_valid_insert
	, prop_valid_remove
	]

test_a :: Test
test_a = testCase "BinaryHeap/test_a" $ HUnit.assertEqual "msg" "foo" "foo"

prop_height ::Test
prop_height = testProperty "BinaryHeap/prop_height" $ \ x -> (x > 0 && x < max_size) ==> (Tree.height (get_tree x)) == (truncate . (logBase 2) . fromIntegral) x

prop_valid_insert :: Test
prop_valid_insert = testProperty "BinaryHeap/prop_valid_insert" $ \ x -> (x > 0 && x < max_size) ==> validate_tree . insert_tree $ x

prop_valid_remove :: Test
prop_valid_remove = testProperty "BinaryHeap/prop_valid_remove" $ \ x -> (x > 0 && x < max_size) ==> validate_tree . remove_tree $ x

-- insert and remove sequential, insert and remove distributed

validate_tree :: (Ord a) => Tree.Tree a -> Bool
validate_tree (Tree.Leaf _) = True
validate_tree (Tree.Branch (Tree.Leaf Tree.Max) _ (Tree.Leaf Tree.Max) size Tree.Max)
	= size == 1
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_size Tree.Max) node (Tree.Leaf Tree.Max) size Tree.Max)
	= (left_node <= node) && (size == left_size + 1) && (validate_tree (Tree.Branch left_left left_node left_right left_size Tree.Max))
validate_tree (Tree.Branch (Tree.Branch left_left left_node left_right left_size Tree.Max) node (Tree.Branch right_left right_node right_right right_size Tree.Max) size Tree.Max)
	= (left_node <= node) && (right_node <= node) && (size == left_size + right_size + 1) && (validate_tree (Tree.Branch left_left left_node left_right left_size Tree.Max)) && (validate_tree (Tree.Branch right_left right_node right_right right_size Tree.Max))