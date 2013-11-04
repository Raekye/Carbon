module Carbon.DataStructures.Queues.PriorityQueue.Scaffolding (get_distributed_queue, max_size) where

import qualified Carbon.DataStructures.Queues.PriorityQueue as PQueue
import qualified Carbon.DataStructures.Trees.NaturalTree as NaturalTree
import Carbon.Testing

max_size :: Integer
max_size = 2 ^ 16

get_distributed_queue :: Integer -> PQueue.PriorityQueue Integer
get_distributed_queue
	= let
		get_distributed_queue' 0 = PQueue.create
		get_distributed_queue' n = let x = distribute_range n in PQueue.insert (get_distributed_queue (n - 1)) x (fromIntegral x)
	in NaturalTree.index (fmap get_distributed_queue' NaturalTree.naturals)