module Carbon.DataStructures.Trees.SelfBalancingBinarySearchTree (Tree (..), create, from_list, to_list, remove, removeall, count, find, size, height, add, prettyprint, rotate_cw, rotate_ccw) where

import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree

import Data.List (foldl')
import Debug.Trace

data Tree a
	= Branch (Tree a) a (Tree a) Int Int
	| Leaf
	deriving (Show)

instance GenericBinaryTree.GenericBinaryTree Tree where
	is_leaf Leaf = True
	is_leaf _ = False
	left (Branch left _ _ _ _) = left
	right (Branch _ _ right _ _) = right
	node (Branch _ node _ _ _) = node
	details (Branch _ _ _ n h) = "N: " ++ (show n) ++ "; H: " ++ (show h)

from_list :: (Ord a) => [a] -> Tree a
from_list li = foldl' (\ tree val -> (add tree val)) create li

to_list :: Tree a -> [a]
to_list Leaf = []
to_list (Branch left node right n _) = (to_list left) ++ (replicate n node) ++ (to_list right)

create :: Tree a
create = Leaf

add :: (Ord a) => Tree a -> a -> Tree a
add (Branch left node right n _) val
	= let (new_left, new_right, new_n)
		| val < node = ((add left val), right, n)
		| val > node = (left, (add right val), n)
		| otherwise = (left, right, (n + 1))
	in balance (Branch new_left node new_right new_n ((max (height new_left) (height new_right)) + 1))
add (Leaf) val = Branch Leaf val Leaf 1 0

balance :: Tree a -> Tree a
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

side :: Tree a -> Int -> Tree a
side (Branch left _ right _ _) s = if (s > 0) then right else left

remove :: (Ord a) => Tree a -> a -> Tree a
remove (Branch left node right n h) val
	| ((val == node) && (n == 1)) = removeall (Branch left node right n h) val
	| otherwise = balance $ Branch new_left node new_right new_n ((max (height new_left) (height new_right)) + 1) 
		where (new_left, new_right, new_n)
			| val < node = ((remove left val), right, n)
			| val > node = (left, (remove right val), n)
			| otherwise = (left, right, n - 1)
remove (Leaf) _ = Leaf

removeall :: (Ord a) => Tree a -> a -> Tree a
removeall (Branch Leaf node Leaf n h) val
	| node == val = Leaf
	| otherwise = (Branch Leaf node Leaf n h)
removeall (Branch left node right n h) val
	= balance $ Branch new_left new_node new_right new_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right, new_node, new_n)
			| val < node = ((removeall left val), right, node, n)
			| val > node = (left, (removeall right val), node, n)
			| otherwise = let
				pop_'root (Leaf) _ = trace "POP ROOT LEAF" (Leaf, (node, n))
				pop_'root (Branch Leaf node Leaf n _) _ = (Leaf, (node, n))
				pop_'root (Branch _ node Leaf n _) 1 = (Leaf, (node, n))
				pop_'root (Branch Leaf node _ n _) 0 = (Leaf, (node, n))
				pop_'root (Branch left node right n _) side
					= let
						ret = pop_'root (if side == 0 then left else right) side
						(left', right') = if side == 0 then ((fst ret), right) else (left, (fst ret))
					in (balance (Branch left' node right' n ((max (height left') (height right')) + 1)), (snd ret))
				factor = balance_factor (Branch left node right n h)
				ret = if factor < 0 then (pop_'root left 1) else (pop_'root right 0)
				root' = snd ret
				(left', right') = if factor < 0 then ((fst ret), right) else (left, (fst ret)) -- TODO: optimize branch with earlier call?
			in (left', right', (fst root'), (snd root')) --(left, right, 0)
removeall (Leaf) _ = Leaf

count :: (Ord a) => Tree a -> a -> Int
count (Branch left node right n _) val
	| val == node = n
	| val > node = count right val
	| otherwise = count left val
count (Leaf) _ = 0

find :: Tree a -> (a -> Int) -> Int
find (Branch left node right n _) f
	| f node == 0 = n
	| f node < 0 = find right f
	| f node > 0 = find left f
find (Leaf) _ = 0

size :: Tree a -> Int
size (Branch left _ right n _) = (size left) + (size right) + (min n 1)
size (Leaf) = 0

height :: Tree a -> Int
height (Branch _ _ _ _ h) = h
height (Leaf) = -1

prettyprint :: Show a => Tree a -> String
prettyprint (Leaf) = "{}"
prettyprint (Branch left node right n h) = GenericBinaryTree.prettyprint (Branch left node right n h)

balance_factor :: Tree a -> Int
balance_factor (Branch left _ right _ _) = (height right) - (height left)
balance_factor (Leaf) = 0

rotate :: Tree a -> Int -> Tree a
rotate (Branch left node right n h) rotation
	| rotation > 0 = rotate_ccw (Branch left node right n h)
	| rotation < 0 = rotate_cw (Branch left node right n h)
	| otherwise = (Branch left node right n h)

rotate_cw :: Tree a -> Tree a
rotate_cw (Branch (Branch left_left left_node left_right left_n _) node right n _)
	= Branch new_left left_node new_right left_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right) = (left_left, (Branch left_right node right n ((max (height left_right) (height right)) + 1)))

rotate_ccw :: Tree a -> Tree a
rotate_ccw (Branch left node (Branch right_left right_node right_right right_n _) n _)
	= Branch new_left right_node new_right right_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right) = ((Branch left node right_left n ((max (height left) (height right_left)) + 1)), right_right)