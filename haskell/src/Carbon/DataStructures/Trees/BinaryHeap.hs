module Carbon.DataStructures.Trees.BinaryHeap (Side, Tree (..), create, insert) where

data Side = LEFT | RIGHT deriving (Show)

data Tree a = Branch {
		left :: Tree a
		, node :: a
		, right :: Tree a
		, next :: Side
	} | Leaf {
	} deriving (Show)


create = Leaf

insert :: Tree a -> a -> Tree a
insert Leaf val = Branch Leaf val Leaf LEFT
insert (Branch left node right next) val
	= fst $ insert_helper (Branch left node right next) val
		where
			insert_helper (Branch left node right next) val
				= let
					bubble_down = smaller val
					new_val = larger val
					bubble_side = left or right node
					ret = recursive call
					new_left = same or new
					new_right = same or new
					loaded_state = next + ret_discharge
					new_discharge = quot loaded_state 2
					new_state = mod loaded_state 2
				in (Branch new_left new_val new_right new_state)
			insert_helper Leaf val = (insert Leaf val, 1)

{-
1. Find next side
2. Compare with root node
3. If greater, set as new root node
4. insert into next node
helper_ret = [depends on what is next], sum = next + discharge, self_discharge = quote discharge 2, new_next = mod discharge 2, return = Branch [left] node [right] next
-}