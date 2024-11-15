-- |
-- Module      : Benchmarks.ByteStringLazy
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.ByteStringLazy where

import Benchmarks.DefaultMain (defaultMain)
-- import Benchmarks.Common (value, maxValue, appendValue)
import Prelude (Int, (+), ($), (.), even, (>), (<=), subtract, undefined,
                maxBound, Maybe(..))
import qualified Prelude as P
import Data.Word (Word8)
import Data.Int (Int64) -- for lazy bytestring

import qualified Data.ByteString.Lazy as S

nAppends :: Int
nElements :: Int64
nElements = 1000000
nAppends = 10000

minElem, maxElem :: Word8
minElem = 1
maxElem = maxBound

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Element = Word8
type Stream a = S.ByteString

{-# INLINE sourceN #-}
sourceN :: Int -> Int -> Stream Element
sourceN count begin = S.unfoldr step begin
    where
    step i =
        if i > begin + count
        then Nothing
        else (Just (P.fromIntegral i, i + 1))

{-# INLINE source #-}
source :: Int -> Stream Element
source = sourceN (P.fromIntegral nElements)

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Int -> ()
appendSourceR n =
    toNull $ P.foldr (S.append) S.empty (P.map (S.singleton . P.fromIntegral) [n..n+nAppends])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> ()
appendSourceL n =
    toNull $ P.foldl (S.append) S.empty (P.map (S.singleton . P.fromIntegral) [n..n+nAppends])

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
toNull :: Stream Element -> ()
toNull = eval

{-# INLINE toList #-}
toList :: Stream Element -> ()
toList = evalF . S.unpack

{-# INLINE foldl #-}
foldl :: Stream Element -> Element
foldl  = S.foldl' (+) 0

{-# INLINE last #-}
last  :: Stream Element -> Element
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Stream a -> ()
transform = eval

{-# INLINE composeN #-}
composeN :: Int
         -> (Stream Element -> Stream Element)
         -> Stream Element
         -> ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

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
map, mapM,
    filterEven, filterAllOut, filterAllIn,
    takeOne, takeAll, takeWhileTrue,
    dropOne, dropAll, dropWhileTrue, dropWhileFalse
    :: Int -> Stream Int -> ()

-- XXX there is no scanl'
-- XXX All scan ops hang for lazy bytestring, disabled for now
{-
{-# INLINE scan #-}
scan :: Int -> Stream Int -> ()
scan           n = composeN n $ S.scanl (+) 0
-}
map            n = composeN n $ S.map (+1)
mapM             = map
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxElem)
filterAllIn    n = composeN n $ S.filter (<= maxElem)
takeOne        n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take nElements
takeWhileTrue  n = composeN n $ S.takeWhile (<= maxElem)
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop nElements
dropWhileFalse n = composeN n $ S.dropWhile (> maxElem)
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxElem)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

iterStreamLen, maxIters :: Int
iterStreamLen = 10
maxIters = 100000

{-# INLINE iterateSource #-}
iterateSource :: (Stream Element -> Stream Element)
              -> Int
              -> Int
              -> Stream Element
iterateSource g i n = f i (sourceN iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

{-# INLINE iterateFilterEven #-}
{-# INLINE iterateTakeAll #-}
{-# INLINE iterateDropOne #-}
{-# INLINE iterateDropWhileFalse #-}
{-# INLINE iterateDropWhileTrue #-}
iterateFilterEven, iterateTakeAll, iterateDropOne,
    iterateDropWhileFalse, iterateDropWhileTrue :: Int -> Stream Element

-- this is quadratic
-- XXX using scanl instead of scanl'
-- XXX All scan ops hang for lazy bytestring, disabled for now
{-
-- Scan increases the size of the stream by 1, drop 1 to not blow up the size
-- due to many iterations.
{-# INLINE iterateScan #-}
iterateScan :: Int -> Stream Element
iterateScan n = iterateSource (error "hangs") maxIters n
-}
iterateFilterEven n = iterateSource (S.filter even) maxIters n
iterateTakeAll n = iterateSource (S.take nElements) maxIters n
iterateDropOne n = iterateSource (S.drop 1) maxIters n
iterateDropWhileFalse n = iterateSource (S.dropWhile (> maxElem)) maxIters n
iterateDropWhileTrue n = iterateSource (S.dropWhile (<= maxElem)) maxIters n

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE dropMap #-}
{-# INLINE takeDrop #-}
{-# INLINE takeMap #-}
{-# INLINE filterDrop #-}
{-# INLINE filterTake #-}
{-# INLINE filterMap #-}
dropMap, takeDrop, takeMap, filterDrop,
    filterTake, filterMap
    :: Int -> Stream Element -> ()

-- XXX using scanl instead of scanl'
-- XXX All scan ops hang for lazy bytestring, disabled for now
-- {-# INLINE scanMap #-}
-- scanMap    n = composeN n $ id
dropMap    n = composeN n $ S.map (subtract 1) . S.drop 1
-- {-# INLINE dropScan #-}
-- dropScan   n = composeN n $ id
takeDrop   n = composeN n $ S.drop 1 . S.take nElements
-- {-# INLINE takeScan #-}
-- takeScan   n = composeN n $ id
takeMap    n = composeN n $ S.map (subtract 1) . S.take nElements
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxElem)
filterTake n = composeN n $ S.take nElements . S.filter (<= maxElem)
-- {-# INLINE filterScan #-}
-- filterScan n = composeN n $ id
filterMap  n = composeN n $ S.map (subtract 1) . S.filter (<= maxElem)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Stream Element -> ()
zip src = P.foldr (\(x,y) xs -> P.seq x (P.seq y xs)) ()
    $ S.zipWith (,) src src

{-# INLINE concatMap #-}
concatMap :: Stream Element -> ()
concatMap src = transform $ (S.concatMap (S.replicate 3) src)

main :: P.IO ()
main = $(defaultMain "ByteStringLazy")
