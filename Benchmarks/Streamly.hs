-- |
-- Module      : Benchmarks.Streamly
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

module Benchmarks.Streamly where

import Benchmarks.Common (value, maxValue)
import Control.Monad (void)
import Prelude
       (Monad, Int, (+), id, ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined)

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

source :: Int -> S.StreamT m Int
source n = S.each [n..n+value]

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

toNull, toList, foldl, last, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Int -> m ()

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

eliminate :: Monad m => (S.StreamT m Int -> m a) -> Int -> m ()
eliminate f = void . f . source

toNull = eliminate $ S.runStreamT
toList = eliminate $ S.toList
foldl  = eliminate $ S.foldl (+) 0 id
last   = eliminate $ S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

transform :: Monad m => (S.StreamT m Int -> S.StreamT m a) -> Int -> m ()
transform f = S.runStreamT . f . source

scan          = transform $ S.scan (+) 0 id
map           = transform $ fmap (+1)
mapM          = transform $ S.mapM return
filterEven    = transform $ S.filter even
filterAllOut  = transform $ S.filter (> maxValue)
filterAllIn   = transform $ S.filter (<= maxValue)
takeOne       = transform $ S.take 1
takeAll       = transform $ S.take maxValue
takeWhileTrue = transform $ S.takeWhile (<= maxValue)
dropAll       = transform $ S.drop maxValue
dropWhileTrue = transform $ S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

zip n         = S.runStreamT $ (S.zipWith (,) (source n) (source n))

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

compose :: Monad m => (S.StreamT m Int -> S.StreamT m Int) -> Int -> m ()
compose f = transform $ (f . f . f . f)

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.filter (<= maxValue) . fmap (subtract 1))

composeScaling :: Monad m => Int -> Int -> m ()
composeScaling m n =
    case m of
        1 -> transform f n
        2 -> transform (f . f) n
        3 -> transform (f . f . f) n
        4 -> transform (f . f . f . f) n
        _ -> undefined
    where f = S.filter (<= maxValue)
