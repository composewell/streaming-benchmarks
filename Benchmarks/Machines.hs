-- |
-- Module      : Benchmarks.Machines
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE RankNTypes #-}
module Benchmarks.Machines where

import Benchmarks.Common (value, maxValue)
import Control.Monad (void)
import Prelude
       (Monad, Int, (+), ($), (.), return, even, (>), (<=),
        subtract, undefined, replicate)

import qualified Data.Machine      as S

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE filterEven #-}
{-# INLINE mapM #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE zip #-}
{-# INLINE concat #-}
{-# INLINE composeMapM #-}
{-# INLINE composeAllInFilters #-}
{-# INLINE composeAllOutFilters #-}
{-# INLINE composeMapAllInFilter #-}
toNull, toList, foldl, last, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Int -> m ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m i o = S.SourceT m o
type Pipe   m i o = S.ProcessT m i o

source :: Monad m => Int -> Source m () Int
source n = S.enumerateFromTo n (n + value)

runStream :: Monad m => Pipe m Int o -> Int -> m ()
runStream t n = void $ S.runT_ $ (source n) S.~> t

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull = S.runT_ . source
toList = void . S.runT . source
foldl  = runStream $ S.fold (+) 0
last   = runStream $ S.final

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

transform :: Monad m => Pipe m Int o -> Int -> m ()
transform = runStream

scan          = transform $ S.scan (+) 0
map           = transform $ S.mapping (+1)
mapM          = transform $ S.autoM return
filterEven    = transform $ S.filtered even
filterAllOut  = transform $ S.filtered (> maxValue)
filterAllIn   = transform $ S.filtered (<= maxValue)
takeOne       = transform $ S.taking 1
takeAll       = transform $ S.taking maxValue
takeWhileTrue = transform $ S.takingWhile (<= maxValue)
dropAll       = transform $ S.dropping maxValue
dropWhileTrue = transform $ S.droppingWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip n = S.runT_ (S.capT (source n) (source n) S.zipping)
concat = transform (S.mapping (replicate 3) S.~> S.asParts)

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

compose :: Monad m => Pipe m Int Int -> Int -> m ()
compose f = transform $ (f S.~> f S.~> f S.~> f)

composeMapM           = compose (S.autoM return)
composeAllInFilters   = compose (S.filtered (<= maxValue))
composeAllOutFilters  = compose (S.filtered (> maxValue))
composeMapAllInFilter = compose (S.mapping (subtract 1) S.~> S.filtered (<= maxValue))

composeScaling :: Monad m => Int -> Int -> m ()
composeScaling m n =
    case m of
        1 -> transform f n
        2 -> transform (f S.~> f) n
        3 -> transform (f S.~> f S.~> f) n
        4 -> transform (f S.~> f S.~> f S.~> f) n
        _ -> undefined
    where f = S.filtered (<= maxValue)
