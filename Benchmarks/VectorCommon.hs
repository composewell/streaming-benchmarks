-- |
-- Module      : Benchmarks.Vector
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

import Benchmarks.Common (value, maxValue, appendValue)
import Prelude (Int, (+), ($), (.), even, (>), (<=), subtract, undefined,
                maxBound, Maybe(..))
import qualified Prelude as P

#ifdef VECTOR_BOXED
import qualified Data.Vector as S
#define CONSTRAINT(a)
#elif defined (VECTOR_UNBOXED)
#define CONSTRAINT(a) S.Unbox a =>
import qualified Data.Vector.Unboxed as S
#else
#define CONSTRAINT(a) Storable a =>
import Foreign.Storable (Storable)
import qualified Data.Vector.Storable as S
#endif

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream = S.Vector

{-# INLINE source #-}
source :: Int -> Stream Int
-- source v = S.fromList [v..v+value]

source n = let v = S.unfoldr step n in v S.++ S.singleton (S.unsafeLast v)
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceN #-}
sourceN :: Int -> Int -> Stream Int
sourceN count begin = let v = S.unfoldr step begin in v S.++ S.singleton (S.unsafeLast v)
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
    P.foldr (S.++) S.empty (P.map S.singleton [n..n+appendValue])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> Stream Int
appendSourceL n = P.foldl (S.++) S.empty (P.map S.singleton [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- Using NFData for evaluation may be fraught with problems because of a
-- non-optimal implementation of NFData instance. So we just evaluate each
-- element of the stream using a fold.
{-# INLINE eval #-}
eval :: CONSTRAINT(a) Stream a -> ()
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
foldl  = S.foldl' (+) 0

{-# INLINE last #-}
last  :: Stream Int -> Int
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: CONSTRAINT(a) Stream a -> ()
transform = eval

{-# INLINE composeN #-}
composeN :: Int -> (Stream Int -> Stream Int) -> Stream Int -> ()
composeN n f x =
    case n of
        1 -> (transform . f) x
        2 -> (transform . f . f) x
        3 -> (transform . f . f . f) x
        4 -> let v = (f . f . f . f) x in transform (v S.++ S.singleton (S.unsafeLast v))
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

#define STORABLE_VECTOR
{-# INLINE zip #-}
#ifndef STORABLE_VECTOR
zip :: Stream Int -> ()
zip src = P.foldr (\(x,y) xs -> P.seq x (P.seq y xs)) ()
    $ S.zipWith (,) src src
#else
zip :: Stream Int -> Stream Int
zip src = S.zipWith (+) src src
#endif

{-# INLINE concatMap #-}
concatMap :: Stream Int -> ()
concatMap src = transform $ (S.concatMap (S.replicate 3) src)
