module Carbon.DataStructures.Trees.BinaryHeap (Tree (..), create, insert, height) where

import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree

data Tree a = Branch {
		left :: Tree a
		, node :: a
		, right :: Tree a
		, next :: Integer
	} | Leaf {
	} deriving (Show)

instance GenericBinaryTree.GenericBinaryTree Tree where
	is_leaf Leaf = True
	is_leaf _ = False
	left (Branch left node right next) = left
	right (Branch left node right next) = right
	node (Branch left node right next) = node
create = Leaf

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf val = Branch Leaf val Leaf 0
insert (Branch left node right next) val
	= fst $ insert_helper (Branch left node right next) val
		where
			insert_helper (Branch left node right next) val
				= let
					(bubble_down, new_val) = if val > node then (node, val) else (val, node)
					bubble_side = if (next == 0) then left else right
					ret = insert_helper bubble_side bubble_down
					(new_left, new_right) = if (next == 0) then ((fst ret), right) else (left, (fst ret)) -- TODO: able to optimize this with earlier branch?
					loaded_state = next + (snd ret)
					new_discharge = quot loaded_state 2
					new_state = mod loaded_state 2
				in ((Branch new_left new_val new_right new_state), new_discharge)
			insert_helper Leaf val = (insert Leaf val, 1)

height :: Tree a -> Integer
height Leaf = -1
height (Branch left node right next) = 1 + (height left)

prettyprint :: Show a => Tree a -> String
prettyprint (Leaf) = "{}"
prettyprint (Branch left node right next) = GenericBinaryTree.prettyprint (Branch left node right next)