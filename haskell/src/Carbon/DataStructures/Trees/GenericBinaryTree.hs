module Carbon.DataStructures.Trees.GenericBinaryTree (Tree (..), GenericBinaryTree (..), prettyprint) where

data Tree a
	= Branch (Tree a) a (Tree a)
	| Leaf
	deriving (Show)

class GenericBinaryTree a where
	is_leaf :: a b -> Bool
	left :: a b -> a b
	right :: a b -> a b
	node :: a b -> b

-- TODO: use pointfree notation
-- see: http://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell

prettyprint :: (GenericBinaryTree a, Show b) => a b -> String
prettyprint tree
	| is_leaf tree = "{}"
	| otherwise = unlines (prettyprint_helper tree)

prettyprint_helper :: (GenericBinaryTree a, Show b) => a b -> [String]
prettyprint_helper tree
	| is_leaf tree = []
	| otherwise = ("{" ++ (show (node tree)) ++ "}") : (prettyprint_subtree (left tree) (right tree))
		where
			prettyprint_subtree left right =
				((pad " +-" " | ") (prettyprint_helper right)) ++ ((pad " `-" "   ") (prettyprint_helper left))
			pad first rest = zipWith (++) (first : repeat rest)