-- |
-- Module      : Benchmarks.Streamly
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-identities #-}

module Benchmarks.Streamly where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Prelude
       (Monad, Int, (+), ($), (.), return, even, (>), (<=),
        subtract, undefined, Maybe(..), maxBound, fmap)
import qualified Prelude as P

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.StreamK as K

-- import Data.Word (Word8)

-- To compare with ByteString we use an element of type Word8
-- It does not seem to make any perceptible difference though
type Element = Int
-- type Element = Word8

nElements :: Int
nElements = maxValue

maxElem :: Element
maxElem = maxBound

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

type Stream m a = S.Stream m a
type StreamK m a = K.StreamK m a

{-# INLINE source #-}
source :: Monad m => Int -> Stream m Element
-- source n = S.fromFoldable [n..n+value]
source n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (P.fromIntegral cnt, cnt + 1))
        {-
source n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))
            -}

{-# INLINE sourceN #-}
sourceN :: Monad m => Int -> Int -> StreamK m Element
sourceN count begin = K.fromStream $ S.unfoldrM step begin
    where
    step i =
        if i > begin + count
        then return Nothing
        else return (Just (P.fromIntegral i, i + 1))

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: Monad m => Int -> Stream m Element
sourceIntFromThenTo n =
    fmap P.fromIntegral $ S.enumerateFromThenTo n (n + 1) (n + value)

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Int -> P.IO ()
appendSourceR n =
    K.drain $ P.foldr K.append K.nil (P.map K.fromPure [n..n+appendValue])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> P.IO ()
appendSourceL n =
    K.drain $ P.foldl K.append K.nil (P.map K.fromPure [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.fold Fold.drain

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
toNull :: Monad m => Stream m Element -> m ()
toList :: Monad m => Stream m Element -> m [Element]
foldl  :: Monad m => Stream m Element -> m Element
last   :: Monad m => Stream m Element -> m (Maybe Element)

toNull = runStream
toList = S.toList
foldl  = S.fold (Fold.foldl' (+) 0)
last   = S.fold Fold.latest

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (Stream m Element -> Stream m Element) -> Stream m Element -> m ()
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
    :: Monad m => Int -> Stream m Element -> m ()

mapM :: Monad m => Int -> Stream m Element -> m ()

scan           n = composeN n $ S.scan (Fold.foldl' (+) 0)
map            n = composeN n $ fmap (+1)
mapM           n = composeN n $ S.mapM return
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
iterateSource
    :: Monad m
    => (StreamK m Element -> StreamK m Element)
    -> Int
    -> Int
    -> StreamK m Element
iterateSource g i n = f i (sourceN iterStreamLen n)

    where

    f (0 :: Int) m = g m
    f x m = g (f (x P.- 1) m)

iterateMapM, iterateScan, iterateFilterEven, iterateTakeAll, iterateDropOne,
    iterateDropWhileFalse, iterateDropWhileTrue :: Monad m => Int -> Stream m Element

-- Scan increases the size of the stream by 1, drop 1 to not blow up the size
-- due to many iterations.
iterateScan n = K.toStream $ iterateSource (K.fromStream . S.drop 1 . S.scan (Fold.foldl' (+) 0) . K.toStream) maxIters n
-- iterateScan n = K.toStream $ iterateSource (K.drop 1 . K.scanl' (+) 0) maxIters n

-- iterateMapM n = K.toStream $ iterateSource (K.fromStream . S.mapM return . K.toStream) maxIters n
iterateMapM n = K.toStream $ iterateSource (K.mapM return) maxIters n

-- The D version is very slow, investigate why.
-- iterateDropWhileFalse n = K.toStream $ iterateSource (K.fromStream . S.dropWhile (> maxElem) . K.toStream) maxIters n
iterateDropWhileFalse n = K.toStream $ iterateSource (K.dropWhile (> maxElem)) maxIters n

-- iterateTakeAll n = K.toStream $ iterateSource (K.fromStream . S.take nElements . K.toStream) maxIters n
iterateTakeAll n = K.toStream $ iterateSource (K.take nElements) maxIters n

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
    :: Monad m => Int -> Stream m Element -> m ()

scanMap    n = composeN n $ fmap (subtract 1) . S.scan (Fold.foldl' (+) 0)
dropMap    n = composeN n $ fmap (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scan (Fold.foldl' (+) 0) . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take nElements
takeScan   n = composeN n $ S.scan (Fold.foldl' (+) 0) . S.take nElements
takeMap    n = composeN n $ fmap (subtract 1) . S.take nElements
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxElem)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxElem)
filterScan n = composeN n $ S.scan (Fold.foldl' (+) 0) . S.filter (<= maxElem)
filterMap  n = composeN n $ fmap (subtract 1) . S.filter (<= maxElem)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Monad m => Stream m Element -> m ()
zip src = transform $ (S.zipWith (,) src src)

{-# INLINE concatMap #-}
concatMap :: Monad m => Stream m Element -> m ()
concatMap src = transform $ (S.concatMap (S.replicate 3) src)

-- XXX composed zip and concat

main :: P.IO ()
main = $(defaultMain "Streamly")
