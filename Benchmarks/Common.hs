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
import System.Random (randomRIO)

import Gauge

value, maxValue :: Int
value = 1000000
maxValue = value + 1000

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases. This happens
-- specially in case of conduit, perhaps because of fusion?
benchIO :: NFData b => String -> (Int -> IO b) -> Int -> Benchmark
benchIO name f _n = bench name $ nfIO $ randomRIO (1,1000) >>= f

benchId :: NFData b => String -> (a -> Identity b) -> a -> Benchmark
benchId name f n = bench name $ nf (runIdentity . f) n
