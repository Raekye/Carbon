module Carbon.DataStructures.Queues.PriorityQueue (PriorityQueue (..), create, insert, pull) where

import qualified Carbon.DataStructures.Trees.BinaryHeap as Heap

data Box a = Box a Int deriving (Eq, Show)

instance Eq a => Ord (Box a) where
	compare (Box _ a) (Box _ b) = if a < b
		then LT else if a > b
			then GT
			else EQ

data PriorityQueue a = PriorityQueue (Heap.Tree (Box a)) deriving (Show)

create :: PriorityQueue a
create = (PriorityQueue (Heap.create Heap.Max))

insert :: (Ord a) => PriorityQueue a -> a -> Int -> PriorityQueue a
insert (PriorityQueue heap) val priority = (PriorityQueue (Heap.insert heap (Box val priority)))

pull :: (Ord a) => PriorityQueue a -> (PriorityQueue a, Maybe a)
pull (PriorityQueue heap)
	= let
		(heap', ret) = (Heap.remove heap)
	in case ret of
		(Just (Box val _)) -> ((PriorityQueue heap'), Just val)
		Nothing -> ((PriorityQueue heap'), Nothing)