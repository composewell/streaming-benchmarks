-- |
-- Module      : Benchmarks.DList
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.DList where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, appendValue)
import Prelude (Int, (+), ($), (.), (>), undefined, Maybe(..))
import qualified Prelude as P
import qualified Data.Foldable as P

import qualified Data.DList          as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream = S.DList

{-# INLINE source #-}
source :: Int -> Stream Int
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

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Int -> Stream Int
appendSourceR n = P.foldr S.append S.empty (P.map S.singleton [n..n+appendValue])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> Stream Int
appendSourceL n = P.foldl S.append S.empty (P.map S.singleton [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- Using NFData for evaluation may be fraught with problems because of a
-- non-optimal implementation of NFData instance. So we just evaluate each
-- element of the stream using a fold.
{-# INLINE eval #-}
eval :: Stream a -> ()
eval = S.foldr P.seq ()

-- eval foldable
{-# INLINE evalF #-}
evalF :: P.Foldable t => t a -> ()
evalF = P.foldr P.seq ()

{-# INLINE toNull #-}
toNull :: Stream Int -> ()
toNull = eval

{-# INLINE toList #-}
toList :: Stream Int -> ()
toList = evalF . S.toList

{-# INLINE foldl #-}
foldl :: Stream Int -> Int
foldl  = P.foldl' (+) 0

{-# INLINE last #-}
last  :: Stream Int -> Int
last   = undefined

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
map            n = composeN n $ S.map (+1)
mapM             = map
filterEven       = undefined
filterAllOut     = undefined
filterAllIn      = undefined
takeOne          = undefined
takeAll          = undefined
takeWhileTrue    = undefined
dropOne          = undefined
dropAll          = undefined
dropWhileFalse   = undefined
dropWhileTrue    = undefined

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
iterateScan   = undefined
iterateDropWhileFalse   = undefined

iterateFilterEven   = undefined
iterateTakeAll   = undefined
iterateDropOne   = undefined
iterateDropWhileTrue   = undefined

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

scanMap      = undefined
dropMap      = undefined
dropScan     = undefined
takeDrop     = undefined
takeScan     = undefined
takeMap      = undefined
filterDrop   = undefined
filterTake   = undefined
filterScan   = undefined
filterMap    = undefined

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Stream Int -> ()
zip _src       = undefined

{-# INLINE concat #-}
concat :: Stream Int -> ()
concat _src    = undefined

main :: P.IO ()
main = $(defaultMain "DList")
