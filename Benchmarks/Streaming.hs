-- |
-- Module      : Benchmarks.Streaming
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Benchmarks.Streaming where

import Benchmarks.Common (value, maxValue)
import Control.DeepSeq (NFData)
import Prelude
       (Monad, Int, (+), id, ($), (.), return, even, (>), (<=),
        subtract, undefined, Maybe, Either(..), foldMap)
--import Prelude (replicate)

import qualified Streaming.Prelude as S

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
    => Stream m Int -> m ()

toList :: Monad m => Stream m Int -> m (S.Of [Int] ())
foldl :: Monad m => Stream m Int -> m (S.Of Int ())
last :: Monad m => Stream m Int -> m (S.Of (Maybe Int) ())

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- Orphan instance to use nfIO on streaming
instance (NFData a, NFData b) => NFData (S.Of a b)

type Stream m a = S.Stream (S.Of a) m ()

{-# INLINE source #-}
source :: Monad m => Int -> Stream m Int
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
appendSource :: Monad m => Int -> Stream m Int
appendSource n = foldMap S.yield [n..n+value]

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.mapM_ (\_ -> return ())

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull = runStream
toList = S.toList
foldl  = S.fold (+) 0 id
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

scan          = transform . S.scan (+) 0 id
map           = transform . S.map (+1)
mapM          = transform . S.mapM return
filterEven    = transform . S.filter even
filterAllOut  = transform . S.filter (> maxValue)
filterAllIn   = transform . S.filter (<= maxValue)
takeOne       = transform . S.take 1
takeAll       = transform . S.take maxValue
takeWhileTrue = transform . S.takeWhile (<= maxValue)
dropAll       = transform . S.drop maxValue
dropWhileTrue = transform . S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip src         = runStream $ (S.zip src src)
concat _src     = return ()
    -- it just hangs with 100% CPU usage
    -- runStream $ (S.concat $ S.map (replicate 3) (source n))

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: Monad m => (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
compose f = transform . f . f . f . f

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.filter (<= maxValue) . S.map (subtract 1))
composeDropOne        = compose (S.drop 1)

composeScaling :: Monad m => Int -> Stream m Int -> m ()
composeScaling m =
    case m of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined
    where f = S.filter (<= maxValue)
