module Carbon.DataStructures.Queues.PriorityQueue () where

data Box a = Box {
		val :: a
		priority :: Int
	}

instance Ord Box where
	compare a b = if a < b
		then LT else if a > b
			then GT
			else EQ

data PriorityQueue a = PriorityQueue {
		
	}