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
remove branch@(Branch l n r c) val
	= branch
remove Leaf _ = Leaf

--remove_min :: (Ord a) => Tree a -> (Tree a, Maybe a)
--remove_min branch@(Branch l n r c)
--	| 
--remove_min Leaf
--	=  (Leaf, Nothing)

do_remove_min :: (Ord a) => Tree a -> (Tree a, Maybe a)
do_remove_min branch@(Branch l n r c)
	= (l, Just n)
do_remove_min Leaf
	= (Leaf, Nothing)

contains :: (Ord a) => Tree a -> a -> Bool
contains branch@(Branch l n r _) val
	| n < val = contains l val
	| n > val = contains r val
	| otherwise = True
contains Leaf _ = False

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
add tree val
	= let
		(Branch left' node' right' _) = do_add tree val
	in (Branch left' node' right' Black) -- root always black

do_add :: (Ord a) => Tree a -> a -> Tree a
do_add branch@(Branch left node right colour) val
	= let branch'
		| val < node = (Branch (add left val) node right colour)
		| val > node = (Branch left node (add right val) colour)
		| otherwise = branch
	in fix_up branch'
do_add Leaf val = (Branch Leaf val Leaf Black)

get_left_node :: Tree a -> Tree a
get_left_node (Branch left _ _ _) = left
get_left_node Leaf = Leaf

get_right_node :: Tree a -> Tree a
get_right_node (Branch _ _ right _) = right
get_right_node Leaf = Leaf

fix_up :: Tree a -> Tree a
fix_up branch
	= on_predicate_branch [is_red] [is_red] flip_colours .
	on_predicate_branch [is_red, is_red . get_left_node] [] rotate_right .
	on_predicate_branch [not . is_red] [is_red] rotate_left $ branch

on_predicate_branch :: [Tree a -> Bool] -> [Tree a -> Bool] -> (Tree a -> Tree a) -> Tree a -> Tree a
on_predicate_branch lps rps f b
	= on_predicate (\ x -> (all ($ get_left_node b) lps) && (all ($ get_right_node b) rps)) f b -- the dollar sign "reverses" the order of the function application

on_predicate :: (a -> Bool) -> (a -> a) -> a -> a
on_predicate p f x
	| p x = f x
	| otherwise = x

rotate_left :: Tree a -> Tree a
rotate_left (Branch left node (Branch rl rn rr rc) colour)
	= let
		left' = (Branch left node rl Red)
		centre' = (Branch left' rn rr colour)
	in centre'

rotate_right :: Tree a -> Tree a
rotate_right (Branch (Branch ll ln lr lc) node right colour)
	= let
		right' = (Branch lr node right Red)
		centre' = (Branch ll ln right' colour)
	in centre'

flip_colours :: Tree a -> Tree a
flip_colours (Branch (Branch ll ln lr lc) node (Branch rl rn rr rc) colour) = let
		left' = (Branch ll ln lr (invert_colour lc))
		right' = (Branch rl rn rr (invert_colour rc))
		centre' = (Branch left' node right' (invert_colour colour))
	in centre'

is_red :: Tree a -> Bool
is_red (Branch _ _ _ Red) = True
is_red _ = False

is_black :: Tree a -> Bool
is_black node = not $ is_red node