-- |
-- Module      : Benchmarks.Sequence
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.Sequence where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Prelude (Int, (+), ($), (.), even, (>), (<=), subtract, undefined,
                Maybe(..))
import qualified Prelude as P
import qualified Data.Foldable as P

import qualified Data.Sequence as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream = S.Seq

{-# INLINE source #-}
source :: Int -> Stream Int
-- source v = S.fromList [v..v+value]

source n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceN #-}
sourceN :: Int -> Int -> Stream Int
sourceN count begin = S.unfoldr step begin
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
appendSourceR n =
    P.foldr (S.><) S.empty (P.map S.singleton [n..n+appendValue])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> Stream Int
appendSourceL n = P.foldl (S.><) S.empty (P.map S.singleton [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- Using NFData for evaluation may be fraught with problems because of a
-- non-optimal implementation of NFData instance. So we just evaluate each
-- element of the stream using a fold.
{-# INLINE eval #-}
eval :: Stream a -> ()
eval = P.foldr P.seq ()

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
toNull :: Stream Int -> ()
toList :: Stream Int -> ()
foldl :: Stream Int -> Int
last  :: Stream Int -> Int

toNull = eval
toList = P.foldr P.seq () . P.toList
foldl  = P.foldl' (+) 0
last xs =
    case S.viewr xs of
        _ S.:> x -> x
        _ -> undefined

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

scan             = undefined
map            n = composeN n $ P.fmap (+1)
mapM             = map
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxValue)
filterAllIn    n = composeN n $ S.filter (<= maxValue)
takeOne        n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take maxValue
takeWhileTrue  n = composeN n $ S.takeWhileL (<= maxValue)
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileFalse n = composeN n $ S.dropWhileL (> maxValue)
dropWhileTrue  n = composeN n $ S.dropWhileL (<= maxValue)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

iterStreamLen, maxIters :: Int
iterStreamLen = 10
maxIters = 100000

{-# INLINE iterateSource #-}
iterateSource :: (Stream Int -> Stream Int) -> Int -> Int -> Stream Int
iterateSource g i n = f i (sourceN iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

{-# INLINE iterateScan #-}
{-# INLINE iterateFilterEven #-}
{-# INLINE iterateTakeAll #-}
{-# INLINE iterateDropOne #-}
{-# INLINE iterateDropWhileFalse #-}
{-# INLINE iterateDropWhileTrue #-}
iterateScan, iterateFilterEven, iterateTakeAll, iterateDropOne,
    iterateDropWhileFalse, iterateDropWhileTrue :: Int -> Stream Int

-- this is quadratic
iterateScan  = undefined
iterateDropWhileFalse n =
    iterateSource (S.dropWhileL (> maxValue)) (maxIters `P.div` 100) n

iterateFilterEven n = iterateSource (S.filter even) maxIters n
iterateTakeAll n = iterateSource (S.take maxValue) maxIters n
iterateDropOne n = iterateSource (S.drop 1) maxIters n
iterateDropWhileTrue n = iterateSource (S.dropWhileL (<= maxValue)) maxIters n

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

scanMap      = undefined -- composeN n $ S.map (subtract 1) . S.scanl' (+) 0
dropMap    n = composeN n $ P.fmap (subtract 1) . S.drop 1
dropScan     = undefined -- composeN n $ S.scanl' (+) 0 . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take maxValue
takeScan     = undefined -- composeN n $ S.scanl' (+) 0 . S.take maxValue
takeMap    n = composeN n $ P.fmap (subtract 1) . S.take maxValue
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxValue)
filterScan   = undefined -- composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)
filterMap  n = composeN n $ P.fmap (subtract 1) . S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Stream Int -> ()
zip src       = transform $ (S.zipWith (,) src src)

{-# INLINE concat #-}
concat :: Stream Int -> Stream Int
concat _src    = undefined -- transform $ (S.concatMap (S.replicate 3) src)

main :: P.IO ()
main = $(defaultMain "Sequence")
