-- |
-- Module      : Benchmarks.Streaming
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Benchmarks.Streaming where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Control.DeepSeq (NFData)
import Prelude
       (Monad, Int, (+), id, ($), (.), return, even, (>), (<=),
        subtract, undefined, Maybe, Either(..), foldMap, maxBound)
import qualified Prelude as P
import Data.Semigroup ((<>))
--import Prelude (replicate)

import qualified Streaming.Prelude as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- Orphan instance to use nfIO on streaming
instance (NFData a, NFData b) => NFData (S.Of a b)

type Stream m a = S.Stream (S.Of a) m ()

{-# INLINE source #-}
source :: Monad m => Int -> Stream m Int
-- source n = S.each [n..n+value]
source n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then return $ Left ()
        else return (Right (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Monad m => Int -> Stream m Int
appendSourceR n = foldMap S.yield [n..n+appendValue]

{-# INLINE appendSourceL #-}
appendSourceL :: Monad m => Int -> Stream m Int
appendSourceL n = P.foldl (<>) P.mempty (P.map S.yield [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.mapM_ (\_ -> return ())

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
toNull :: Monad m => Stream m Int -> m ()
toList :: Monad m => Stream m Int -> m (S.Of [Int] ())
foldl :: Monad m => Stream m Int -> m (S.Of Int ())
last :: Monad m => Stream m Int -> m (S.Of (Maybe Int) ())

toNull = runStream
toList = S.toList
foldl  = S.fold (+) 0 id
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
scan, map, mapM,
    filterEven, filterAllOut, filterAllIn,
    takeOne, takeAll, takeWhileTrue,
    dropOne, dropAll, dropWhileTrue, dropWhileFalse
    :: Monad m => Int -> Stream m Int -> m ()

scan           n = composeN n $ S.scan (+) 0 id
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
dropWhileFalse n = composeN n $ S.dropWhile (> maxValue)
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

scanMap    n = composeN n $ S.map (subtract 1) . S.scan (+) 0 id
dropMap    n = composeN n $ S.map (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scan (+) 0 id . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take maxValue
takeScan   n = composeN n $ S.scan (+) 0 id . S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) . S.take maxValue
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxValue)
filterScan n = composeN n $ S.scan (+) 0 id . S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) . S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Monad m => Stream m Int -> m ()
zip src = runStream $ (S.zip src src)

{-# INLINE concatMapFoldable #-}
concatMapFoldable :: Monad m => Stream m Int -> m ()
concatMapFoldable src = runStream $ (S.concat $ S.map (P.replicate 3) src)

main :: P.IO ()
main = $(defaultMain "Streaming")
