-- |
-- Module      : Benchmarks.Common
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

module Benchmarks.Common
    ( value
    , maxValue
    , benchIO
    , benchId
    ) where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Data.Functor.Identity (Identity, runIdentity)

import Gauge

value, maxValue :: Int
value = 1000000
maxValue = value + 1000

benchIO :: NFData b => String -> (a -> IO b) -> a -> Benchmark
benchIO name f n = bench name $ nfIO $ f n >>= return

benchId :: NFData b => String -> (a -> Identity b) -> a -> Benchmark
benchId name f n = bench name $ nf (runIdentity . f) n
