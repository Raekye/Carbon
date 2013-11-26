module Carbon.DataStructures.Graphs.AdjacencyList (AdjacencyList (..), create, are_adjacent, neighbors, add, delete, add_node, delete_node) where

import Data.List (foldl')
import Data.Maybe (Maybe (Just, Nothing))

data Node a = Node a [a]

data AdjacencyList a = AdjacencyList [Node a]

create :: AdjacencyList a
create = (AdjacencyList [])

are_adjacent:: (Ord a) => AdjacencyList a -> a -> a -> Bool
are_adjacent (AdjacencyList nodes) a b
	= let
		node_contains (Just ((car : cdr), other))
			| car == other = True
			| otherwise = node_contains (Just (cdr, other))
		node_contains (Just ([], _))
			= False
		node_contains Nothing
			= False
		find_oneof ((Node car neighbors) : cdr) a b
			| car == a = Just (neighbors, b)
			| car == b = Just (neighbors, a)
			| otherwise = find_oneof cdr a b
		find_oneof [] _ _ = Nothing
	in node_contains $ find_oneof nodes a b

neighbors :: (Ord a) => AdjacencyList a -> a -> [a]
neighbors (AdjacencyList nodes) a
	= let
		(Node _ neighbors) = find_node nodes a
	in neighbors

find_node :: (Ord a) => [Node a] -> a -> Node a
find_node ((Node car neighbors) : cdr) a
	= if car == a then (Node car neighbors) else (find_node cdr a)
find_node [] _
	= undefined

add :: AdjacencyList a -> a -> a -> AdjacencyList a
add (AdjacencyList nodes) a b
	= let
	in undefined

delete :: AdjacencyList a -> a -> a -> AdjacencyList a
delete (AdjacencyList nodes) a b
	= let
	in undefined

add_node :: AdjacencyList a -> a -> AdjacencyList a
add_node (AdjacencyList nodes) added_node = (AdjacencyList ((Node added_node []) : nodes))

delete_node :: (Ord a) => AdjacencyList a -> a -> AdjacencyList a
delete_node (AdjacencyList nodes) deleted_node
	= let
		fold_fn aggregate (Node x neighbors)
			| x == deleted_node = aggregate
			| otherwise = (Node x (unlink_deleted_edge neighbors)) : aggregate
		unlink_deleted_edge (car : cdr)
			= let
				rest = unlink_deleted_edge cdr
			in if car == deleted_node then rest else car : rest
		unlink_deleted_edge [] = []
	in (AdjacencyList (foldl' fold_fn [] nodes))