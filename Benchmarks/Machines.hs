-- |
-- Module      : Benchmarks.Machines
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE RankNTypes #-}
module Benchmarks.Machines where

import Benchmarks.Common (value, maxValue)
import Prelude
       (Monad, Int, (+), ($), return, even, (>), (<=),
        subtract, replicate, Maybe(..))

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
toNull, foldl, last, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => S.MachineT m k Int -> m ()

toList :: Monad m => S.MachineT m k Int -> m [Int]

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m o = S.SourceT m o
type Pipe   m i o = S.ProcessT m i o

source :: Monad m => Int -> Source m Int
-- source n = S.source [n..n+value]
source n = S.unfoldT step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE runStream #-}
runStream :: Monad m => Pipe m Int o -> S.MachineT m k Int -> m ()
runStream t src = S.runT_ $ src S.~> t

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull = S.runT_
toList = S.runT
foldl  = runStream $ S.fold (+) 0
last   = runStream $ S.final

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Pipe m Int o -> S.MachineT m k Int -> m ()
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

zip _src = S.runT_ (S.capT (source 10) (source 20) S.zipping)
concat = transform (S.mapping (replicate 3) S.~> S.asParts)

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

compose :: Monad m => Pipe m Int Int -> S.MachineT m k Int -> m ()
compose f = transform $ (f S.~> f S.~> f S.~> f)

composeMapM           = compose (S.autoM return)
composeAllInFilters   = compose (S.filtered (<= maxValue))
composeAllOutFilters  = compose (S.filtered (> maxValue))
composeMapAllInFilter = compose (S.mapping (subtract 1) S.~> S.filtered (<= maxValue))

composeScaling :: Monad m => Int -> Source m Int -> m ()
composeScaling m =
    case m of
        1 -> transform f
        2 -> transform (f S.~> f)
        3 -> transform (f S.~> f S.~> f)
        4 -> transform (f S.~> f S.~> f S.~> f)
    --    _ -> undefined
    where f = S.filtered (<= maxValue)
