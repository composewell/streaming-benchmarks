-- |
-- Module      : Benchmarks.StreamlyArray
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Benchmarks.StreamlyArray where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Prelude (Int, (+), ($), (.), even, (>), (<=), subtract, undefined,
                maxBound, Maybe(..), fmap, div)
import qualified Prelude as P

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue) -- , appendValue)

import qualified Streamly.Data.Stream  as S
import qualified Streamly.Data.Array as A
import qualified Streamly.Data.Fold as Fold

instance NFData (A.Array a) where
    {-# INLINE rnf #-}
    rnf _ = ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream = A.Array

{-# INLINE source #-}
source :: MonadIO m => Int -> m (Stream Int)
source n = S.fold (A.writeN value) (S.unfoldr step n)
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceN #-}
sourceN :: MonadIO m => Int -> Int -> m (Stream Int)
sourceN count begin = S.fold (A.writeN value) (S.unfoldr step begin)
    where
    step i =
        if i > begin + count
        then Nothing
        else (Just (i, i + 1))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-
{-# INLINE appendSourceR #-}
appendSourceR :: Int -> Stream Int
appendSourceR n =
    P.foldr (S.++) S.empty (P.map S.singleton [n..n+appendValue])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> Stream Int
appendSourceL n = P.foldl (S.++) S.empty (P.map S.singleton [n..n+appendValue])
-}

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
toNull :: P.Monad m => Stream Int -> m (Stream Int)
toNull = P.return

{-# INLINE toList #-}
toList :: P.Monad m => Stream Int -> m ([Int])
toList = P.return . A.toList

{-# INLINE foldl #-}
foldl :: MonadIO m => Stream Int -> m Int
foldl = S.fold Fold.sum . S.unfold A.reader

{-# INLINE last #-}
last :: P.Monad m => Stream Int -> m (Maybe Int)
last arr = P.return (A.getIndex (A.length arr P.- 1) arr)

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-
{-# INLINE transform #-}
transform :: Stream a -> ()
transform = eval
-}

{-# INLINE composeN #-}
composeN
    :: MonadIO m
    => Int
    -> (S.Stream m Int -> S.Stream m Int)
    -> Stream Int
    -> m (Stream Int)
composeN n f x =
    case n of
        1 -> S.fold A.write $ f $ S.unfold A.reader x
        2 -> S.fold A.write $ f . f $ S.unfold A.reader x
        3 -> S.fold A.write $ f . f . f $ S.unfold A.reader x
        4 -> S.fold A.write $ f . f . f . f $ S.unfold A.reader x
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
scan, map, mapM,
    filterEven, filterAllOut, filterAllIn,
    takeOne, takeAll, takeWhileTrue,
    dropOne, dropAll, dropWhileTrue, dropWhileFalse
    :: MonadIO m => Int -> Stream Int -> m (Stream Int)

scan           n = composeN n $ S.scan (Fold.foldl' (+) 0)
map            n = composeN n $ fmap (+1)
mapM           n = composeN n $ S.mapM (\x -> P.return $ x + 1)
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxValue)
filterAllIn    n = composeN n $ S.filter (<= maxValue)
takeOne        n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take maxValue
takeWhileTrue  n = composeN n $ S.takeWhile (<= maxValue)
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileFalse n = composeN n $ S.dropWhile (> maxValue)
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

iterStreamLen, maxIters :: Int
iterStreamLen = 10
maxIters = 100000

{-# INLINE iterateSource #-}
iterateSource :: MonadIO m
    => (S.Stream m Int -> S.Stream m Int) -> Int -> Int -> m (Stream Int)
iterateSource g i n =
    sourceN iterStreamLen n P.>>= \a -> S.fold A.write (f i $ S.unfold A.reader a)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

{-# INLINE iterateMapM #-}
{-# INLINE iterateScan #-}
{-# INLINE iterateFilterEven #-}
{-# INLINE iterateTakeAll #-}
{-# INLINE iterateDropOne #-}
{-# INLINE iterateDropWhileFalse #-}
{-# INLINE iterateDropWhileTrue #-}
iterateMapM, iterateScan, iterateFilterEven, iterateTakeAll, iterateDropOne,
    iterateDropWhileFalse, iterateDropWhileTrue ::
        MonadIO m => Int -> m (Stream Int)

-- Scan increases the size of the stream by 1, drop 1 to not blow up the size
-- due to many iterations.
iterateScan n = iterateSource (S.drop 1 . S.scan (Fold.foldl' (+) 0)) (maxIters `div` 100) n
iterateMapM n = iterateSource (S.mapM P.return) maxIters n
iterateFilterEven n = iterateSource (S.filter even) maxIters n
iterateTakeAll n = iterateSource (S.take maxValue) maxIters n
iterateDropOne n = iterateSource (S.drop 1) maxIters n
iterateDropWhileFalse n = iterateSource (S.dropWhile (> maxValue)) (maxIters `div` 100) n
iterateDropWhileTrue n = iterateSource (S.dropWhile (<= maxValue)) maxIters n

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
    :: MonadIO m => Int -> Stream Int -> m (Stream Int)

scanMap    n = composeN n $ fmap (subtract 1) . S.scan (Fold.foldl' (+) 0)
dropMap    n = composeN n $ fmap (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scan (Fold.foldl' (+) 0) . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take maxValue
takeScan   n = composeN n $ S.scan (Fold.foldl' (+) 0) . S.take maxValue
takeMap    n = composeN n $ fmap (subtract 1) . S.take maxValue
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxValue)
filterScan n = composeN n $ S.scan (Fold.foldl' (+) 0) . S.filter (<= maxBound)
filterMap  n = composeN n $ fmap (subtract 1) . S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-
{-# INLINE zip #-}
zip :: Stream Int -> ()
zip src       = P.foldr (\(x,y) xs -> P.seq x (P.seq y xs)) ()
    $ S.zipWith (,) src src

{-# INLINE concat #-}
concat :: Stream Int -> ()
concat src    = transform $ (S.concatMap (S.replicate 3) src)
-}

main :: P.IO ()
main = $(defaultMain "StreamlyArray")
