{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree (Tree (..), create, remove, removeall, count, find, size, height, add, prettyprint, rotate_cw, rotate_ccw) where

import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree
import Debug.Trace

data Tree a
	= Branch (Tree a) a (Tree a) Int Int
	| Leaf
	deriving (Show)

instance GenericBinaryTree.GenericBinaryTree Tree where
	is_leaf Leaf = True
	is_leaf _ = False
	left (Branch left node right n h) = left
	right (Branch left node right n h) = right
	node (Branch left node right n h) = node

create = Leaf

add (Branch left node right n h) val
	= let (new_left, new_right, new_n)
		| val < node = ((add left val), right, n)
		| val > node = (left, (add right val), n)
		| otherwise = (left, right, (n + 1))
	in balance (Branch new_left node new_right new_n ((max (height new_left) (height new_right)) + 1))
add (Leaf) val = Branch Leaf val Leaf 1 0

balance (Branch left node right n h)
	= let
		factor = balance_factor (Branch left node right n h)
		sub_factor
			| factor > 0 = balance_factor right
			| factor < 0 = balance_factor left
			| otherwise = 0
		in if (factor /= 2 && factor /= -2)
			then (Branch left node right n h)
			else
				let (new_left, new_right)
					| factor * sub_factor > 0 = (left, right)
					| factor == 2 = (left, (rotate right sub_factor))
					| otherwise = ((rotate left sub_factor), right)
				in rotate (Branch new_left node new_right n ((max (height new_left) (height new_right)) + 1)) factor
balance (Leaf) = Leaf

side (Branch left node right n h) s = if (s > 0) then right else left

remove (Branch left node right n h) val
	| ((val == node) && (n == 1)) = removeall (Branch left node right n h) val
	| otherwise = Branch new_left node new_right new_n ((max (height new_left) (height new_right)) + 1) 
		where (new_left, new_right, new_n)
			| val < node = ((remove left val), right, n)
			| val > node = (left, (remove right val), n)
			| otherwise = (left, right, n - 1)
remove (Leaf) val = Leaf

removeall (Branch left node right n h) val
	= Branch new_left node new_right new_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right, new_n)
			| val < node = ((removeall left val), right, n)
			| val > node = (left, (removeall right val), n)
			| otherwise = (left, right, 0) -- TODO: remove the node when deleted
removeall (Leaf) val = Leaf

clear (Branch left node right n h) = Leaf
clear (Leaf) = Leaf

count (Branch left node right n h) val
	| val == node = n
	| val > node = count right val
	| otherwise = count left val
count (Leaf) val = 0

find (Branch left node right n h) f
	| f node == 0 = n
	| f node < 0 = find right f
	| f node > 0 = find left f
find (Leaf) f = 0

size (Branch left node right n h) = (size left) + (size right) + (min n 1)
size (Leaf) = 0

height (Branch left node right n h) = h
height (Leaf) = -1

prettyprint :: Show a => Tree a -> String
prettyprint (Leaf) = "{}"
prettyprint (Branch left node right n h) = GenericBinaryTree.prettyprint (Branch left node right n h)

balance_factor (Branch left node right n h) = (height right) - (height left)
balance_factor (Leaf) = 0

rotate (Branch left node right n h) rotation
	| rotation > 0 = rotate_ccw (Branch left node right n h)
	| rotation < 0 = rotate_cw (Branch left node right n h)
	| otherwise = (Branch left node right n h)

rotate_cw (Branch (Branch left_left left_node left_right left_n left_h) node right n h)
	= Branch new_left left_node new_right left_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right) = (left_left, (Branch left_right node right n ((max (height left_right) (height right)) + 1)))
rotate_ccw (Branch left node (Branch right_left right_node right_right right_n right_h) n h)
	= Branch new_left right_node new_right right_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right) = ((Branch left node right_left n ((max (height left) (height right_left)) + 1)), right_right)