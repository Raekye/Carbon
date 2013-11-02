module Carbon.DataStructures.Trees.BinaryHeap (Tree (..), create, to_list, from_list, insert, remove, height, size) where

import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree
import Debug.Trace
import Data.List (foldl')

--data Tree a = Branch {
--		left :: Tree a
--		, node :: a
--		, right :: Tree a
--		, size :: Integer
--	} | Leaf {
--	} deriving (Show)

data Tree a
	= Branch (Tree a) a (Tree a) Integer
	| Leaf
	deriving (Show)

instance GenericBinaryTree.GenericBinaryTree Tree where
	is_leaf Leaf = True
	is_leaf _ = False
	left (Branch left _ _ _) = left
	right (Branch _ _ right _) = right
	node (Branch _ node _ _) = node
	details (Branch _ _ _ size) = "S: " ++ (show size)

from_list :: Ord a => [a] -> Tree a
from_list li = foldl' (\ tree val -> (insert tree val)) create li

to_list :: Ord a => Tree a -> [a]
to_list Leaf = []
to_list (Branch left node right size)
	= let
		ret = remove (Branch left node right size)
		Just x = snd ret
	in x : (to_list (fst ret))

create :: Tree a
create = Leaf

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf val = Branch Leaf val Leaf 1
insert (Branch left node right size) val
	= bubble_down (Branch left node right size) val

bubble_down :: Ord a => Tree a -> a -> Tree a
bubble_down Leaf val = insert Leaf val
bubble_down (Branch left node right size_num) val
	= let
		(bubble_down_val, val') = if val > node then (node, val) else (val, node)
		next = derive_side_from_size size_num
		(left', right') = if (next == 0) then ((bubble_down left bubble_down_val), right) else (left, (bubble_down right bubble_down_val))
	in (Branch left' val' right' ((size left') + (size right') + 1))

derive_side_from_size :: Integer -> Int
derive_side_from_size size
	= let
		height = truncate $ logBase 2 (fromIntegral size)
		max_children_in_bottom_row = 2 ^ height
		factor = (mod (size - max_children_in_bottom_row + 1) max_children_in_bottom_row)
	in if factor * 2 < max_children_in_bottom_row then 0 else 1

remove :: Ord a => Tree a -> (Tree a, Maybe a)
remove Leaf = (Leaf, Nothing)
remove (Branch left node right size_num)
	= let
		pop_latest (Branch Leaf node Leaf _) = (Leaf, node)
		pop_latest (Branch left node right size_num)
			= let
				next = derive_side_from_size (size_num - 1)
				(subtree, node') = if (next == 0) then (pop_latest left) else (pop_latest right)
				(bubbling_node, settled_node) = if (node' > node) then (node', node) else (node, node')
				(left', right') = if (next == 0) then (subtree, right) else (left, subtree)
			in ((Branch left' settled_node right' ((size left') + (size right') + 1)), bubbling_node)
		side = derive_side_from_size (size_num - 1)
		(subtree, node') = pop_latest (if side == 0 then left else right)
		(left', right') = if side == 0 then (subtree, right) else (left, subtree) -- TODO: optimize with earlier branch?
	in ((Branch left' node' right' ((size left') + (size right') + 1)), Just node)

height :: Tree a -> Integer
height Leaf = -1
height (Branch _ _ _ size) = truncate $ logBase 2 (fromIntegral size)

size :: Tree a -> Integer
size (Branch _ _ _ size) = size
size Leaf = 0

prettyprint :: Show a => Tree a -> String
prettyprint (Leaf) = "{}"
prettyprint (Branch left node right size) = GenericBinaryTree.prettyprint (Branch left node right size)