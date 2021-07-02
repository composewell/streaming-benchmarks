-- |
-- Module      : Benchmarks.List
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.List where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Prelude (Int, (+), ($), (.), even, (>), (<=), subtract, undefined,
                maxBound, Maybe(..))
import qualified Prelude as P

import qualified Data.List          as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream = []

{-# INLINE source #-}
source :: Int -> [Int]
-- source v = [v..v+value]

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

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: Int -> Stream Int
sourceIntFromThenTo n = P.enumFromThenTo n (n + 1) (n + value)

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Int -> Stream Int
appendSourceR n = P.foldr (S.++) [] (P.map (: []) [n..n+appendValue])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> Stream Int
appendSourceL n = P.foldl (S.++) [] (P.map (: []) [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- Using NFData for evaluation may be fraught with problems because of a
-- non-optimal implementation of NFData instance. So we just evaluate each
-- element of the stream using a fold.
{-# INLINE eval #-}
eval :: Stream a -> ()
eval = S.foldr P.seq ()

{-# INLINE toNull #-}
{-# INLINE toList #-}
toNull, toList :: Stream Int -> ()

toNull = eval
toList = eval

{-# INLINE foldl #-}
{-# INLINE last #-}
foldl, last :: Stream Int -> Int
foldl  = S.foldl' (+) 0
last   = S.last

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

scan           n = composeN n $ S.scanl' (+) 0
map            n = composeN n $ S.map (+1)
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
iterateScan n = iterateSource (S.scanl' (+) 0) (maxIters `P.div` 100) n
iterateDropWhileFalse n =
    iterateSource (S.dropWhile (> maxValue)) (maxIters `P.div` 100) n

iterateFilterEven n = iterateSource (S.filter even) maxIters n
iterateTakeAll n = iterateSource (S.take maxValue) maxIters n
iterateDropOne n = iterateSource (S.drop 1) maxIters n
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
    :: Int -> Stream Int -> ()

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
zip :: Stream Int -> ()
zip src = P.foldr (\(x,y) xs -> P.seq x (P.seq y xs)) ()
    $ S.zipWith (,) src src

{-# INLINE concatMap #-}
concatMap :: Stream Int -> ()
concatMap src = transform $ (S.concatMap (S.replicate 3) src)

main :: P.IO ()
main = $(defaultMain "List")
