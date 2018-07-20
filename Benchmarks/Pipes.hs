-- |
-- Module      : Benchmarks.Pipes
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE RankNTypes #-}

module Benchmarks.Pipes where

import Benchmarks.Common (value, maxValue)
import Data.Void (Void)
import Prelude
       (Monad, Int, (+), ($), id, return, even, (>), (<=),
        subtract, undefined, replicate, Maybe, Either(..), foldMap)

import qualified Pipes             as S
import qualified Pipes.Prelude     as S

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
toNull, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter, composeDropOne
    :: Monad m
    => Source m () Int -> m ()

toList :: Monad m => Source m () Int -> m [Int]
foldl :: Monad m => Source m () Int -> m Int
last :: Monad m => Source m () Int -> m (Maybe Int)

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m i o = S.Producer o m i
type Sink   m i r = S.Proxy () i () Void m r
type Pipe   m i o = S.Proxy () i () o m ()

{-# INLINE source #-}
source :: Monad m => Int -> Source m () Int
-- source n = S.each [n..n+value]
source n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then return $ Left ()
        else return (Right (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSource #-}
appendSource :: Monad m => Int -> Source m () Int
appendSource n = foldMap S.yield [n..n+value]

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull src = S.runEffect $ S.for src S.discard
toList = S.toListM
foldl  = S.fold (+) 0 id
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Pipe m Int Int -> Source m () Int -> m ()
transform t src = S.runEffect $ S.for (src S.>-> t) S.discard

scan          = transform $ S.scan (+) 0 id
map           = transform $ S.map (+1)
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
-- Zipping and concat
-------------------------------------------------------------------------------

zip src = S.runEffect $ S.for (S.zip src src) S.discard
concat = transform (S.map (replicate 3) S.>-> S.concat)

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: Monad m => Pipe m Int Int -> Source m () Int -> m ()
compose f = transform $ (f S.>-> f S.>-> f S.>-> f)

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.map (subtract 1) S.>-> S.filter (<= maxValue))
composeDropOne        = compose (S.drop 1)

composeScaling :: Monad m => Int -> Source m () Int -> m ()
composeScaling m =
    case m of
        1 -> transform f
        2 -> transform (f S.>-> f)
        3 -> transform (f S.>-> f S.>-> f)
        4 -> transform (f S.>-> f S.>-> f S.>-> f)
        _ -> undefined
    where f = S.filter (<= maxValue)
