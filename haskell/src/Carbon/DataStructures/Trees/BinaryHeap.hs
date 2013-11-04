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
insert (Branch left node right size_num strain) val
	= let
		next = derive_side_from_size size_num
		(Branch subtree_left subtree_node subtree_right subtree_size _) = if next == 0 then (insert left val) else (insert right val)
		(node', subtree_node') = if (compare subtree_node node strain) then (subtree_node, node) else (node, subtree_node)
		subtree' = (Branch subtree_left subtree_node' subtree_right subtree_size strain)
		(left', right') = if next == 0 then (subtree', right) else (left, subtree')
	in (Branch left' node' right' ((size left') + (size right') + 1) strain)

derive_side_from_size :: Integer -> Int
derive_side_from_size size
	= let
		height = truncate $ logBase 2 (fromIntegral size)
		max_children_in_bottom_row = 2 ^ height
		factor = (mod (size - max_children_in_bottom_row + 1) max_children_in_bottom_row)
	in if factor * 2 < max_children_in_bottom_row then 0 else 1

-- | returns true if the left element should bubble up, weighted towards right elemnent
compare :: (Ord a) => a -> a -> HeapStrain -> Bool
compare a b Min = a < b
compare a b Max = a > b

remove :: Ord a => Tree a -> (Tree a, Maybe a)
remove (Leaf strain) = ((Leaf strain), Nothing)
remove (Branch left node right size_num strain)
	= let
		pop_latest (Branch (Leaf _) node (Leaf _) _ _) = ((Leaf strain), node)
		pop_latest (Branch left node right size_num _)
			= let
				next = derive_side_from_size (size_num - 1)
				(subtree, latest) = pop_latest $ if next == 0 then left else right
				(left', right') = if next == 0 then (subtree, right) else (left, subtree)
			in ((Branch left' node right' ((size left') + (size right') + 1) strain), latest)
		(subtree, latest) = pop_latest (Branch left node right size_num strain)
		bubble_down (Branch (Leaf _) node (Leaf _) size_num _)
			= (Branch (Leaf strain) node (Leaf strain) size_num strain)
		bubble_down (Branch (Branch left_left left_node left_right left_size_num _) node (Leaf _) size_num strain)
			| compare left_node node strain = (Branch (bubble_down (Branch left_left node left_right left_size_num strain)) left_node (Leaf strain) size_num strain)
			| otherwise = (Branch (Branch left_left left_node left_right left_size_num strain) node (Leaf strain) size_num strain)
		bubble_down (Branch (Branch left_left left_node left_right left_size_num _) node (Branch right_left right_node right_right right_size_num _) size_num _)
			| not (compare left_node right_node strain) = if (compare right_node node strain) then (Branch (Branch left_left left_node left_right left_size_num strain) right_node (bubble_down (Branch right_left node right_right right_size_num strain)) size_num strain) else (Branch (Branch left_left left_node left_right left_size_num strain) node (Branch right_left right_node right_right right_size_num strain) size_num strain)
			| compare left_node node strain = (Branch (bubble_down (Branch left_left node left_right left_size_num strain)) left_node (Branch right_left right_node right_right right_size_num strain) size_num strain)
			| otherwise = (Branch (Branch left_left left_node left_right left_size_num strain) node (Branch right_left right_node right_right right_size_num strain) size_num strain)
	in case subtree of
		(Branch subtree_left _ subtree_right subtree_size_num _) -> ((bubble_down (Branch subtree_left latest subtree_right subtree_size_num strain)), Just node)
		(Leaf _) -> ((Leaf strain), Just node)

height :: Tree a -> Integer
height (Leaf _) = -1
height (Branch _ _ _ size _) = truncate $ logBase 2 (fromIntegral size)

size :: Tree a -> Integer
size (Branch _ _ _ size _) = size
size (Leaf strain) = 0

prettyprint :: Show a => Tree a -> String
prettyprint (Leaf _) = "{}"
prettyprint (Branch left node right size strain) = GenericBinaryTree.prettyprint (Branch left node right size strain)