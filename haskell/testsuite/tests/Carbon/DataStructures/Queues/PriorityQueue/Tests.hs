module Carbon.DataStructures.Queues.PriorityQueue.Tests (tests) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Carbon.DataStructures.Queues.PriorityQueue as PQueue
import Carbon.DataStructures.Queues.PriorityQueue.Scaffolding

tests :: [Test]
tests = [
	prop_insert_pull
	]

prop_insert_pull :: Test
prop_insert_pull = testProperty "BinaryHeap/prop_insert_pull" $ \ x -> (x > 0 && x < max_size) ==>
	let
		(queue', (Just top)) = PQueue.pull . get_distributed_queue $ x
	in ((snd . PQueue.pull) (PQueue.insert queue' top (fromIntegral top))) == Just top