module Carbon.DataStructures.Trees.LeftLeaningRedBlackTree.Tests (tests) where

import Test.QuickCheck
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Carbon.DataStructures.Trees.LeftLeaningRedBlackTree as Tree
import Carbon.DataStructures.Trees.LeftLeaningRedBlackTree.Scaffolding
import Carbon.Testing

import Debug.Trace

tests :: [Test]
tests = [ prop_a
	]

prop_a ::Test
prop_a = testProperty "LeftLeaningRedBlackTree/prop_a" $ \ x -> (x > 0 && x < max_size) ==> Tree.height (get_tree x) >= 0