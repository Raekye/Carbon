module Main where

import Test.QuickCheck
--import Test.HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit


main :: IO ()
main = defaultMain [tests_a]

tests_a :: Test
tests_a = testGroup "Test A" [
	testProperty "Test A - Property A" prop_a
	, testProperty "Test B - Property B" prop_b
	]

prop_a :: Int -> Bool
prop_a x = True

prop_b :: Int -> Bool
prop_b x = True

{-
module AllTests(tests) where

import Distribution.TestSuite

tests = return [ Test succeeds, Test fails ]
	where
		succeeds = TestInstance {
			run = return $ Finished Pass
			, name = "succeeds"
			, tags = []
			, options = []
			, setOption = \_ _ -> Right suceeds
		}
		fails = TestInstance {
			run = return $ Finished $ fail "Always fails!"
			, name = "fails"
			, tags = []
			, options = []
			, setOption = \_ _ -> Right fails
		}
-}
