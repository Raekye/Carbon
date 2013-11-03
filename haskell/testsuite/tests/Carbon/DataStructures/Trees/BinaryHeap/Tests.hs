module Carbon.DataStructures.Trees.BinaryHeap.Tests (tests) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Carbon.DataStructures.Trees.BinaryHeap as Tree
import Data.List

create_tree n = foldl' (\ tree val -> (Tree.insert tree val)) Tree.create [1..n]
max_size = 2 ^ 8

tests :: [Test]
tests = [ test_a
	, prop_b
	, prop_c
	, prop_d
	]

test_a :: Test
test_a = testCase "BinaryHeap/test_a" $ HUnit.assertEqual "msg" "foo" "foo"

prop_c ::Test
prop_c = testProperty "BinaryHeap/prop_c" $ prop_c'

prop_c' :: Int -> Property
prop_c' n = (n > 0 && n < max_size) ==> (Tree.height (create_tree n)) == (truncate (logBase 2 (fromIntegral n)))

prop_d :: Test
prop_d = testProperty "BinaryHeap/prop_d" $ prop_d'

prop_d' :: Int -> Property
prop_d' n = (n < max_size) ==> validate_tree $ create_tree n

-- insert and remove sequential, insert and remove distributed

validate_tree tree = True