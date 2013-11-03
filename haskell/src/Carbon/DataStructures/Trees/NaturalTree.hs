module Carbon.DataStructures.Trees.NaturalTree (index, naturals) where

import qualified Carbon.DataStructures.Trees.GenericBinaryTree as GenericBinaryTree

instance Functor GenericBinaryTree.Tree where
	fmap f (GenericBinaryTree.Branch left node right) = GenericBinaryTree.Branch (fmap f left) (f node) (fmap f right)

index :: GenericBinaryTree.Tree a -> Integer -> a
index (GenericBinaryTree.Branch _ node _) 0 = node
index (GenericBinaryTree.Branch left _ right) n = case (divMod (n - 1) 2) of
	(quotient, 0) -> index left quotient
	(quotient, 1) -> index right quotient

-- naturals = GenericBinaryTree.Branch (fmap ((+ 1) . (* 2)) naturals) 0 (fmap ((* 2) . (+ 1)) naturals)
naturals :: GenericBinaryTree.Tree Integer
naturals = create 0 1 where
	create n step = let
		step' = step * 2
		in seq n $ seq step $ GenericBinaryTree.Branch (create (n + step) step') n (create (n + step') step')