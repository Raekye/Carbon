module Carbon.DataStructures.Trees.BinaryHeap (HeapStrain (..), Tree (..), create, to_list, from_list, insert, remove, height, size) where

import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree

import Data.List (foldl')
import Prelude hiding (compare)
import Debug.Trace

data HeapStrain = Min | Max deriving (Show)

data Tree a
	= Branch (Tree a) a (Tree a) Integer HeapStrain
	| Leaf HeapStrain
	deriving (Show)

instance GenericBinaryTree.GenericBinaryTree Tree where
	is_leaf (Leaf _) = True
	is_leaf _ = False
	left (Branch left _ _ _ _) = left
	right (Branch _ _ right _ _) = right
	node (Branch _ node _ _ _) = node
	details (Branch _ _ _ size _) = "S: " ++ (show size)

from_list :: Ord a => [a] -> HeapStrain -> Tree a
from_list li strain = foldl' (\ tree val -> (insert tree val)) (create strain) li

to_list :: Ord a => Tree a -> [a]
to_list (Leaf _) = []
to_list (Branch left node right size strain)
	= let
		ret = remove (Branch left node right size strain)
		Just x = snd ret
	in x : (to_list (fst ret))

create :: HeapStrain -> Tree a
create strain = (Leaf strain)

-- | Is stable
insert :: Ord a => Tree a -> a -> Tree a
insert (Leaf strain) val = Branch (Leaf strain) val (Leaf strain) 1 strain
insert (Branch left node right size strain) val
	= bubble_down (Branch left node right size strain) val

bubble_down :: Ord a => Tree a -> a -> Tree a
bubble_down (Leaf strain) val = insert (Leaf strain) val
bubble_down (Branch left node right size_num strain) val
	= let
		(bubble_down_val, val') = if (compare val node strain) then (node, val) else (val, node)
		next = derive_side_from_size size_num
		(left', right') = if (next == 0) then ((bubble_down left bubble_down_val), right) else (left, (bubble_down right bubble_down_val))
	in (Branch left' val' right' ((size left') + (size right') + 1) strain)

derive_side_from_size :: Integer -> Int
derive_side_from_size size
	= let
		height = truncate $ logBase 2 (fromIntegral size)
		max_children_in_bottom_row = 2 ^ height
		factor = (mod (size - max_children_in_bottom_row + 1) max_children_in_bottom_row)
	in if factor * 2 < max_children_in_bottom_row then 0 else 1

-- | returns true if the left element should bubble up
compare :: (Ord a) => a -> a -> HeapStrain -> Bool
compare a b Min = a < b
compare a b Max = a > b

remove :: Ord a => Tree a -> (Tree a, Maybe a)
remove (Leaf strain) = ((Leaf strain), Nothing)
remove (Branch (Leaf _) node _ _ strain) = ((Leaf strain), Just node)
remove (Branch left node right size_num strain)
	= let
		pop_latest (Branch (Leaf _) node (Leaf _) _ strain) = ((Leaf strain), node)
		pop_latest (Branch left node right size_num strain)
			= let
				next = derive_side_from_size (size_num - 1)
				(subtree, node') = if (next == 0) then (pop_latest left) else (pop_latest right)
				(bubbling_node, settled_node) = if (compare node' node strain) then (node', node) else (node, node')
				(left', right') = if (next == 0) then (subtree, right) else (left, subtree)
			in ((Branch left' settled_node right' ((size left') + (size right') + 1) strain), bubbling_node)
		side = derive_side_from_size (size_num - 1)
		(subtree, node') = pop_latest (if side == 0 then left else right)
		(left', right') = if side == 0 then (subtree, right) else (left, subtree) -- TODO: optimize with earlier branch?
	in ((Branch left' node' right' ((size left') + (size right') + 1) strain), Just node)

height :: Tree a -> Integer
height (Leaf _) = -1
height (Branch _ _ _ size _) = truncate $ logBase 2 (fromIntegral size)

size :: Tree a -> Integer
size (Branch _ _ _ size _) = size
size (Leaf strain) = 0

prettyprint :: Show a => Tree a -> String
prettyprint (Leaf _) = "{}"
prettyprint (Branch left node right size strain) = GenericBinaryTree.prettyprint (Branch left node right size strain)