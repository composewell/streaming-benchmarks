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
    , benchPure
    ) where

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import Gauge

value, maxValue :: Int
value = 1000000
maxValue = value + 1000

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases. This happens
-- specially in case of conduit, perhaps because of fusion?
{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> a) -> (a -> IO b) -> Benchmark
benchIO name src f = bench name $ nfIO $ randomRIO (1,1000) >>= f . src

{-# INLINE benchId #-}
benchId :: (NFData b) => String -> (Int -> a) -> (a -> Identity b) -> Benchmark
benchId name src f = bench name $ nf (runIdentity . f) (src 10)

{-# INLINE benchPure #-}
benchPure :: NFData b => String -> (Int -> a) -> (a -> b) -> Benchmark
benchPure name src f = bench name $ nf f (src 10)
