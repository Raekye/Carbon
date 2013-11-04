module Carbon.Testing (distribute_range) where

import System.Random

stdgen :: StdGen
stdgen = mkStdGen 0

distribute_range :: Integer -> Integer
distribute_range x = fst $ randomR (0, (2 ^ 3 - 1)) stdgen