-- |
-- Module      : Benchmarks.Pipes
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE RankNTypes #-}

module Benchmarks.Pipes where

import Benchmarks.Common (value, maxValue)
import Control.Monad (void)
import Data.Void (Void)
import Prelude
       (Monad, Int, (+), ($), id, (.), return, even, (>), (<=),
        subtract, undefined, replicate)

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
toNull, toList, foldl, last, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Int -> m ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m i o = S.Producer' o m i
type Sink   m i r = S.Proxy () i () Void m r
type Pipe   m i o = S.Proxy () i () o m ()

source :: Monad m => Int -> Source m () Int
source n = S.each [n..n+value]

{-# INLINE runStream #-}
runStream :: Monad m => Sink m Int () -> Int -> m ()
runStream t n = void $ S.runEffect $ (source n) S.>-> t

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

eliminate :: Monad m => (S.Proxy Void i () Int m () -> m a) -> Int -> m ()
eliminate s = void . s . source

toNull = runStream $ S.mapM_ (\_ -> return ())
toList = eliminate $ S.toListM
foldl  = eliminate $ S.fold (+) 0 id
last   = eliminate $ S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-- discard vs mapM
{-# INLINE transform #-}
transform :: Monad m => Pipe m Int Int -> Int -> m ()
transform t = runStream (t S.>-> S.mapM_ (\_ -> return ()))

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

zip n = S.runEffect $ S.for (S.zip (source n) (source n)) S.discard
concat = transform (S.map (replicate 3) S.>-> S.concat)

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: Monad m => Pipe m Int Int -> Int -> m ()
compose f = transform $ (f S.>-> f S.>-> f S.>-> f)

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.map (subtract 1) S.>-> S.filter (<= maxValue))

composeScaling :: Monad m => Int -> Int -> m ()
composeScaling m n =
    case m of
        1 -> transform f n
        2 -> transform (f S.>-> f) n
        3 -> transform (f S.>-> f S.>-> f) n
        4 -> transform (f S.>-> f S.>-> f S.>-> f) n
        _ -> undefined
    where f = S.filter (<= maxValue)
