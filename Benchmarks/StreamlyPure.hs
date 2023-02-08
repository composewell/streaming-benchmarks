-- |
-- Module      : Benchmarks.StreamlyPure
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.StreamlyPure where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Data.Functor.Identity (Identity, runIdentity)
import Prelude
       (Int, (+), ($), (.), even, (>), (<=),
        subtract, undefined, Maybe(..), foldMap, maxBound, fmap)
import qualified Prelude as P

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.StreamK as K

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

type Stream = S.Stream Identity
type StreamK = K.StreamK Identity

maxElem :: Int
maxElem = maxBound

{-# INLINE source #-}
source :: Int -> Stream Int
source n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceN #-}
sourceN :: Int -> Int -> StreamK Int
sourceN count begin = K.fromStream $ S.unfoldr step begin
    where
    step i =
        if i > begin + count
        then Nothing
        else (Just (i, i + 1))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Int -> Stream Int
appendSourceR n = K.toStream $ foldMap K.fromPure [n..n+appendValue]

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> Stream Int
appendSourceL n =
    K.toStream $ P.foldl (P.<>) K.nil (P.map K.fromPure [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- Using NFData for evaluation may be fraught with problems because of a
-- non-optimal implementation of NFData instance. So we just evaluate each
-- element of the stream using a fold.
{-# INLINE eval #-}
eval :: Stream a -> ()
eval = runIdentity . S.foldrM P.seq (P.return ())

-- eval foldable
{-# INLINE evalF #-}
evalF :: P.Foldable t => t a -> ()
evalF = P.foldr P.seq ()

{-# INLINE toNull #-}
toNull :: Stream Int -> ()
toNull = eval

{-# INLINE toList #-}
toList :: Stream Int -> ()
toList = evalF . runIdentity . S.toList

{-# INLINE foldl #-}
foldl :: Stream Int -> Int
foldl = runIdentity . S.fold (Fold.foldl' (+) 0)

{-# INLINE last #-}
last :: Stream Int -> Maybe Int
last = runIdentity . S.fold Fold.latest

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Stream a -> ()
transform = eval

{-# INLINE composeN #-}
composeN :: Int -> (Stream Int -> Stream Int) -> Stream Int -> ()

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
scan, map, mapM,
    filterEven, filterAllOut, filterAllIn,
    takeOne, takeAll, takeWhileTrue,
    dropOne, dropAll, dropWhileTrue, dropWhileFalse
    :: Int -> Stream Int -> ()

scan           n = composeN n $ S.scan (Fold.foldl' (+) 0)
map            n = composeN n $ fmap (+1)
mapM             = map
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
iterateSource
    :: (StreamK Int -> StreamK Int)
    -> Int
    -> Int
    -> StreamK Int
iterateSource g i n = f i (sourceN iterStreamLen n)

    where

    f (0 :: Int) m = g m
    f x m = g (f (x P.- 1) m)

iterateMapM, iterateScan, iterateFilterEven, iterateTakeAll, iterateDropOne,
    iterateDropWhileFalse, iterateDropWhileTrue :: Int -> Stream Int

-- Scan increases the size of the stream by 1, drop 1 to not blow up the size
-- due to many iterations.
iterateScan n = K.toStream $ iterateSource (K.fromStream . S.drop 1 . S.scan (Fold.foldl' (+) 0) . K.toStream) (maxIters `P.div` 100) n
-- iterateScan n = K.toStream $ iterateSource (K.scanl' (+) 0) (maxIters `div` 100) n

-- iterateMapM n = K.toStream $ iterateSource (K.fromStream . S.mapM return . K.toStream) maxIters n
iterateMapM n = K.toStream $ iterateSource (K.mapM P.return) maxIters n

-- The D version is very slow, investigate why.
-- iterateDropWhileFalse n = K.toStream $ iterateSource (K.fromStream . S.dropWhile (> maxElem) . K.toStream) maxIters n
iterateDropWhileFalse n = K.toStream $ iterateSource (K.dropWhile (> maxElem)) maxIters n

-- iterateTakeAll n = K.toStream $ iterateSource (K.fromStream . S.take maxValue . K.toStream) maxIters n
iterateTakeAll n = K.toStream $ iterateSource (K.take maxValue) maxIters n

iterateFilterEven n = K.toStream $ iterateSource (K.fromStream . S.filter even . K.toStream) maxIters n
-- iterateFilterEven n = K.toStream $ iterateSource (K.filter even) maxIters n

iterateDropOne n = K.toStream $ iterateSource (K.fromStream . S.drop 1 . K.toStream) maxIters n
-- iterateDropOne n = K.toStream $ iterateSource (K.drop 1) maxIters n

-- iterateDropWhileTrue n = K.toStream $ iterateSource (K.fromStream . S.dropWhile (<= maxElem) . K.toStream) maxIters n
iterateDropWhileTrue n = K.toStream $ iterateSource (K.dropWhile (<= maxElem)) maxIters n

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
    :: Int -> Stream Int -> ()

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

{-# INLINE zip #-}
zip :: Stream Int -> ()
zip src = runIdentity $ S.foldr (\(x,y) xs -> P.seq x (P.seq y xs)) ()
    $ S.zipWith (,) src src

{-# INLINE concatMap #-}
concatMap :: Stream Int -> ()
concatMap src = transform $ (S.concatMap (S.replicate 3) src)

main :: P.IO ()
main = $(defaultMain "StreamlyPure")
