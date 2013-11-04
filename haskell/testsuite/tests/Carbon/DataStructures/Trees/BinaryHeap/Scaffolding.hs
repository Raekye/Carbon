module Carbon.DataStructures.Trees.BinaryHeap.Scaffolding (max_size, get_tree, get_distributed_tree, insert_tree, remove_tree) where

import qualified Carbon.DataStructures.Trees.BinaryHeap as Tree
import qualified Carbon.DataStructures.Trees.NaturalTree as NaturalTree
import Carbon.Testing

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
		get_distributed_tree' 0 = Tree.create Tree.Max
		get_distributed_tree' n = Tree.insert (get_distributed_tree (n - 1)) (distribute_range n)
	in NaturalTree.index (fmap get_distributed_tree' NaturalTree.naturals)

insert_tree :: Integer -> Tree.Tree Integer
insert_tree n
	= Tree.insert (get_tree (n - 1)) n

remove_tree :: Integer -> Tree.Tree Integer
remove_tree n
	= fst . Tree.remove . get_tree $ n