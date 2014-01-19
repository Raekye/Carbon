module Carbon.DataStructures.Trees.LeftLeaningRedBlackTree (Tree (..), create, from_list, to_list, remove, find, contains, size, height, add, prettyprint, rotate_left, rotate_right, flip_colours) where

import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree

import Data.List (foldl')
import Debug.Trace

data Colour = Red | Black deriving (Show)

data Tree a
	= Branch (Tree a) a (Tree a) Colour
	| Leaf
	deriving (Show)

instance GenericBinaryTree.GenericBinaryTree Tree where
	is_leaf Leaf = True
	is_leaf _ = False
	left (Branch left _ _ _) = left
	right (Branch _ _ right _ ) = right
	node (Branch _ node _ _ ) = node
	details (Branch _ _ _ colour) = "C: " ++ (show colour)

remove :: (Ord a) => Tree a -> a -> Tree a
remove _ _ = Leaf

contains :: (Ord a) => Tree a -> a -> Bool
contains _ _ = False

find :: Tree a -> (a -> Int) -> Maybe a
find _ _ = Nothing

size :: Tree a -> Int
size _ = 0

height :: Tree a -> Int
height _ = 0

prettyprint :: Show a => Tree a -> String
prettyprint _ = ""

invert_colour :: Colour -> Colour
invert_colour Red = Black
invert_colour Black = Red

from_list :: (Ord a) => [a] -> Tree a
from_list li = foldl' (\ tree val -> (add tree val)) create li

to_list :: Tree a -> [a]
to_list Leaf = []
to_list (Branch left node right _) = (to_list left) ++ [node] ++ (to_list right)

create :: Tree a
create = Leaf

add :: (Ord a) => Tree a -> a -> Tree a
add (Branch left node right colour) val
	= let
		get_left_node (Branch left _ _ _)
			= left
		do_insert (Branch left node right colour) val
			= let
				(Branch left' _ right' colour') = insert_side (Branch left node right colour) val
				(Branch left'' _ right'' colour'') = if ((not (is_red right')) && (is_red right')) then (rotate_left (Branch left' node right' colour')) else (Branch left' node right' colour')
				centre''' = if ((is_red left') && (is_red (get_left_node left''))) then (rotate_right (Branch left'' node right'' colour'')) else (Branch left'' node right'' colour'')
			in centre'''
		insert_side (Branch left node right colour) val
			| val < node = do_insert left val
			| val > node = do_insert right val
			| otherwise = (Branch left' node right' colour')
			where (Branch left' _ right' colour') = if ((is_red left) && (is_red right)) then (flip_colours (Branch left node right colour)) else (Branch left node right colour) -- if move after rotations becomes 2-3 trees
		insert_side Leaf val
			= (Branch Leaf val Leaf Red) -- new nodes always red
		(Branch left' _ right' _) = do_insert (Branch left node right colour) val
	in (Branch left' node right' Black) -- root always black

rotate_left :: Tree a -> Tree a
rotate_left (Branch left node (Branch right_left right_node right_right right_colour) colour)
	= let
		left' = (Branch left node right_left Red)
		centre' = (Branch left' right_node right_right colour)
	in centre'

rotate_right :: Tree a -> Tree a
rotate_right (Branch (Branch left_left left_node left_right left_colour) node right colour)
	= let
		right' = (Branch left_right node right Red)
		centre' = (Branch left_left left_node right' colour)
	in centre'

flip_colours :: Tree a -> Tree a
flip_colours (Branch (Branch left_left left_node left_right left_colour) node (Branch right_left right_node right_right right_colour) colour) = let
		left' = (Branch left_left left_node left_right (invert_colour left_colour))
		right' = (Branch right_left right_node right_right (invert_colour right_colour))
		centre' = (Branch left' node right' (invert_colour colour))
	in centre'

is_red :: Tree a -> Bool
is_red (Branch _ _ _ Red) = True
is_red Leaf = False

is_black :: Tree a -> Bool
is_black node = not $ is_red node