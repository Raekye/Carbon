module Carbon.DataStructures.Trees.LeftLeaningRedBlackTree.Scaffolding (get_tree, get_distributed_tree, insert_distributed_tree, search_tree, remove_tree, max_size, golden_ratio) where

import qualified Carbon.DataStructures.Trees.LeftLeaningRedBlackTree as Tree
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
		get_tree' n = Tree.add (get_tree (n - 1)) n
	in NaturalTree.index (fmap get_tree' NaturalTree.naturals)

get_distributed_tree :: Integer -> Tree.Tree Integer
get_distributed_tree
	= let
		get_distributed_tree' 0 = Tree.create
		get_distributed_tree' n = Tree.add (get_distributed_tree (n - 1)) (distribute_range n)
	in NaturalTree.index (fmap get_distributed_tree' NaturalTree.naturals)

insert_distributed_tree :: Integer -> Tree.Tree Integer
insert_distributed_tree n
	= if n > 0 then (Tree.add (get_distributed_tree (n - 1)) (distribute_range n)) else get_tree 0

search_tree :: Integer -> Bool
search_tree n = Tree.contains (get_tree n) n

remove_tree :: Integer -> Tree.Tree Integer
remove_tree n
	= if n > 0 then (Tree.remove (get_tree (n - 1)) (distribute_range n)) else get_tree 0