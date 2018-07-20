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
        subtract, undefined, Maybe(..), foldMap, maxBound)
import qualified Prelude as P

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

-------------------------------------------------------------------------------
-- Stream generation
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

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
toNull :: Monad m => Stream m Int -> m ()
toList :: Monad m => Stream m Int -> m [Int]
foldl  :: Monad m => Stream m Int -> m Int
last   :: Monad m => Stream m Int -> m (Maybe Int)

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

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE mapM #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropOne #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileFalse #-}
scan, map,
    filterEven, filterAllOut, filterAllIn,
    takeOne, takeAll, takeWhileTrue,
    dropOne, dropAll, dropWhileTrue, dropWhileFalse
    :: Monad m => Int -> Stream m Int -> m ()

mapM :: S.MonadAsync m => Int -> Stream m Int -> m ()

scan           n = composeN n $ S.scanl' (+) 0
map            n = composeN n $ S.map (+1)
mapM           n = composeN n $ S.mapM return
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxValue)
filterAllIn    n = composeN n $ S.filter (<= maxValue)
takeOne        n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take maxValue
takeWhileTrue  n = composeN n $ S.takeWhile (<= maxValue)
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileFalse n = composeN n $ S.dropWhile (<= 1)
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
{-# INLINE dropMap #-}
{-# INLINE dropScan #-}
{-# INLINE takeDrop #-}
{-# INLINE takeScan #-}
{-# INLINE takeMap #-}
{-# INLINE filterDrop #-}
{-# INLINE filterTake #-}
{-# INLINE filterScan #-}
{-# INLINE filterMap #-}
scanMap, dropMap, dropScan, takeDrop, takeScan, takeMap, filterDrop,
    filterTake, filterScan, filterMap
    :: Monad m => Int -> Stream m Int -> m ()

scanMap    n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0
dropMap    n = composeN n $ S.map (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scanl' (+) 0 . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take maxValue
takeScan   n = composeN n $ S.scanl' (+) 0 . S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) . S.take maxValue
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxValue)
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) . S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
{-# INLINE concat #-}
zip, concat :: Monad m => Stream m Int -> m ()

zip src       = transform $ (S.zipWith (,) src src)
concat _n     = return ()

-- XXX composed zip and concat
