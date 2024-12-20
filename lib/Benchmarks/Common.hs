-- |
-- Module      : Benchmarks.Common
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

module Benchmarks.Common
    ( value
    , maxValue
    , appendValue
    , benchIO
    , benchIOAction
    , benchIOArray
    , benchId
    , benchPure
    , benchPureFunc
    ) where

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import Gauge

value, maxValue,appendValue :: Int
value = 1000000
maxValue = value + 1000
appendValue = 10000

{-# INLINE benchIOAction #-}
benchIOAction :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIOAction name action = bench name $ nfIO $ randomRIO (1,1) >>= action

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> a) -> (a -> IO b) -> Benchmark
benchIO name src f = bench name $ nfIO $ randomRIO (1,1) >>= f . src

{-# INLINE benchId #-}
benchId :: (NFData b) => String -> (Int -> a) -> (a -> Identity b) -> Benchmark
benchId name src f = bench name $ nf (runIdentity . f) (src 10)

{-# INLINE benchPureFunc #-}
benchPureFunc :: NFData b => String -> (Int -> b) -> Benchmark
benchPureFunc name action =
    bench name $ nfIO $ randomRIO (1,1) >>= return . action

{-# INLINE benchPure #-}
benchPure :: (NFData b) => String -> (Int -> a) -> (a -> b) -> Benchmark
benchPure name src f =
    bench name $ nfIO $ randomRIO (1,1) >>= return . f . src

{-# INLINE benchIOArray #-}
benchIOArray :: NFData b => String -> (Int -> IO a) -> (a -> IO b) -> Benchmark
benchIOArray name src f =
    bench name $ nfIO $ randomRIO (1,1) >>= src >>= f
