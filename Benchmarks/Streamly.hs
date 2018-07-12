-- |
-- Module      : Benchmarks.Streamly
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}
module Benchmarks.Streamly where

import Benchmarks.Common (value, maxValue)
import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined, Maybe(..), foldMap)
import qualified Prelude as P

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

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
toNull, scan, map, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Stream m Int -> m ()

mapM, composeMapM :: S.MonadAsync m => Stream m Int -> m ()
toList :: Monad m => Stream m Int -> m [Int]
foldl :: Monad m => Stream m Int -> m Int
last :: Monad m => Stream m Int -> m (Maybe Int)

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.SerialT m a

{-# INLINE source #-}
source :: S.MonadAsync m => Int -> Stream m Int
-- source n = S.fromFoldable [n..n+value]
source n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))
        {-
source n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))
            -}

{-# INLINE sourceN #-}
sourceN :: S.MonadAsync m => Int -> Int -> Stream m Int
sourceN count begin = S.unfoldrM step begin
    where
    step i =
        if i > begin + count
        then return Nothing
        else return (Just (i, i + 1))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSource #-}
appendSource :: Monad m => Int -> Stream m Int
appendSource n = foldMap (S.yieldM . return) [n..n+value]

{-# INLINE mapMSource #-}
mapMSource :: S.MonadAsync m => Int -> Stream m Int
mapMSource n = f 100000 (sourceN 10 n)
    where
        f :: S.MonadAsync m => Int -> Stream m Int -> Stream m Int
        f 0 m = S.mapM return m
        f x m = S.mapM return (f (x P.- 1) m)

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull = runStream
toList = S.toList
foldl  = S.foldl' (+) 0
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

scan          = transform . S.scanl' (+) 0
map           = transform . fmap (+1)
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

zip src       = transform $ (S.zipWith (,) src src)
concat _n     = return ()

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: Monad m => (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
compose f = transform . f . f . f . f

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.filter (<= maxValue) . fmap (subtract 1))

composeScaling :: Monad m => Int -> Stream m Int -> m ()
composeScaling m =
    case m of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined
    where f = S.filter (<= maxValue)
