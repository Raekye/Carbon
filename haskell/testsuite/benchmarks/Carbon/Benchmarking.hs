module Carbon.Benchmarking (benchmark_function_with_values) where

import qualified Criterion.Main as Criterion

benchmark_function_with_values name fn values = map (\ x -> Criterion.bench (name ++ "-" ++ (show x)) (Criterion.whnf fn x)) values