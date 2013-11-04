module Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree.Scaffolding (get_tree, insert_tree, search_tree, remove_tree, max_size, golden_ratio) where

import qualified Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree as Tree
import qualified Carbon.DataStructures.Trees.NaturalTree as NaturalTree
import Carbon.Testing

max_size :: Integer
max_size = 2 ^ 16

golden_ratio :: Double
golden_ratio = (1 + (sqrt 5)) / 2

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