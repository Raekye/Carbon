module DataStructures.Trees.SelfBalancingBinaryTree (Tree, create, remove, removeall, count, find, size, height, add, prettyprint, rotate_cw, rotate_ccw) where

import Prelude hiding (Left, Right)

data Tree a
	= Branch (Tree a) a (Tree a) Int Int
	| Leaf
	deriving (Show)

data TreePosition = FIRST | MIDDLE | LAST deriving (Eq)

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
			| factor > 0 = balance_factor new_right
			| factor < 0 = balance_factor new_left
			| otherwise = 0
		in case factor of
			-2 -> if (sub_factor == -1) then Leaf else Leaf
			2 -> if (sub_factor == 1) then Leaf else Leaf
balance (Leaf) = Leaf

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
			| otherwise = (Leaf, Leaf, 0) -- TODO
removeall (Leaf) val = Leaf

clear (Branch left node right n h) = Leaf
clear (Leaf) = Leaf

count (Branch left node right n h) val
	| node == val = n
	| val > node = count right val
	| val < node = count left val
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

prettyprint (Branch left node right n h)
	= (show node) ++ (prettyprint_helper left FIRST) ++ (prettyprint_helper right LAST)
prettyprint (Leaf)
	= "Empty tree."
prettyprint_helper (Branch left node right n h) position
	= (if position == FIRST then "\n" else "") ++ (show node) ++ (if position == LAST then "" else ", ") ++ (prettyprint_helper left position) ++ (prettyprint_helper right position)
prettyprint_helper (Leaf) FIRST = "\n, "
prettyprint_helper (Leaf) LAST = "."
prettyprint_helper (Leaf) MIDDLE = ""

balance_factor (Branch left node right n h) = (height right) - (height left)
balance_factor (Leaf) = 0

rotate_cw (Branch (Branch left_left left_node left_right left_n left_h) node right n h)
	= Branch new_left left_node new_right left_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right) = (left_left, (Branch left_right node right n ((max (height left_right) (height right)) + 1)))
rotate_ccw (Branch left node (Branch right_left right_node right_right right_n right_h) n h)
	= Branch new_left right_node new_right right_n ((max (height new_left) (height new_right)) + 1)
		where (new_left, new_right) = ((Branch left node right_left n ((max (height left) (height right_left)) + 1)), right_right)
