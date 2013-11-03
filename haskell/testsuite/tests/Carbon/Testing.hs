module Carbon.Testing (distribute_range) where

distribute_range :: Integer -> Integer
distribute_range x = truncate $ (sin (fromIntegral (x ^ 2))) * (2 ** 32)