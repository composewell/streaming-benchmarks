-- |
-- Module      : Benchmarks.List
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

module Benchmarks.List where

import Benchmarks.Common (value, maxValue)
import Prelude
       (Monad, Int, (+), id, ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined, Maybe)

import qualified Data.List          as S

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
toNull, toList, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue,
    concat, composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: [Int] -> [Int]

foldl :: [Int] -> Int
last  :: [Int] -> Int
zip :: [Int] -> [(Int, Int)]

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

source :: Int -> [Int]
source v = [v..v+value]

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull = id
toList = id
foldl  = S.foldl' (+) 0
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: [a] -> [a]
transform = id

scan          = transform . S.scanl' (+) 0
map           = transform . S.map (+1)
mapM          = map
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
concat src    = transform $ (S.concatMap (S.replicate 3) src)

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: ([Int] -> [Int]) -> [Int] -> [Int]
compose f = transform . f . f . f . f

composeMapM           = compose (S.map (+1))
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.filter (<= maxValue) . S.map (subtract 1))

composeScaling :: Int -> [Int] -> [Int]
composeScaling m =
    case m of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined
    where f = S.filter (<= maxValue)
