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
find_node nodes a
	= let
		(ret, _) = find_node_with_rest [] nodes a
	in ret

find_node_with_rest :: (Ord a) => [Node a] -> [Node a] -> a -> (Node a, ([Node a], [Node a]))
find_node_with_rest prev ((Node car neighbors) : cdr) a
	| car == a = (Node car neighbors, (prev, cdr))
	| otherwise = find_node_with_rest ((Node car neighbors) : prev) cdr a
find_node_with_rest _ [] _ = undefined

find_first_of :: (Ord a) => [Node a] -> [Node a] -> a -> a -> ((Node a, a), ([Node a], [Node a]))
find_first_of prev ((Node car neighbors) : cdr) a b
	| car == a = (((Node car neighbors), b), (prev, cdr))
	| car == b = (((Node car neighbors), a), (prev, cdr))
	| otherwise = find_first_of ((Node car neighbors) : prev) cdr a b
find_first_of _ [] _ _
	= undefined

add :: (Ord a) => AdjacencyList a -> a -> a -> AdjacencyList a
add (AdjacencyList nodes) a b
	= let
		(((Node first neighbors), other), (prev, rest)) = find_first_of [] nodes a b
		first' = case (elem other neighbors) of
			True -> error "Edge already exists"
			False -> (Node first (other : neighbors))
		((Node _ other_neighbors), (prev', rest')) = find_node_with_rest prev rest other
		other' = case (elem first other_neighbors) of
			True -> error "Edge already exists"
			False -> (Node other (first : other_neighbors))
	in (AdjacencyList (first' : other' : (prev' ++ rest')))

delete :: (Ord a) => AdjacencyList a -> a -> a -> AdjacencyList a
delete (AdjacencyList nodes) a b
	= let
		fold_fn_delete :: (Ord a) => a -> [a] -> a -> [a]
		fold_fn_delete to_delete aggregate x
			| x == to_delete = aggregate
			| otherwise = x : aggregate
		(((Node first neighbors), other), (prev, rest)) = find_first_of [] nodes a b
		first' = (Node first (foldl' (fold_fn_delete other) [] neighbors))
		((Node _ other_neighbors), (prev', rest')) = find_node_with_rest prev rest other
		other' = (Node other (foldl' (fold_fn_delete first) [] other_neighbors))
	in (AdjacencyList (first' : other' : (prev' ++ rest')))

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