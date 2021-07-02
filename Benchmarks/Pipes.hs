-- |
-- Module      : Benchmarks.Pipes
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.Pipes where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Data.Void (Void)
import Prelude
       (Monad, Int, (+), ($), id, return, even, (>), (<=),
        subtract, undefined, replicate, Maybe, Either(..), foldMap, maxBound)
import qualified Prelude as P
import Data.Semigroup ((<>))

import qualified Pipes             as S
import qualified Pipes.Prelude     as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m i o = S.Producer o m i
type Sink   m i r = S.Proxy () i () Void m r
type Pipe   m i o = S.Proxy () i () o m ()

{-# INLINE source #-}
source :: Monad m => Int -> Source m () Int
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
appendSourceR :: Monad m => Int -> Source m () Int
appendSourceR n = foldMap S.yield [n..n+appendValue]

{-# INLINE appendSourceL #-}
appendSourceL :: Monad m => Int -> Source m () Int
appendSourceL n = P.foldl (<>) P.mempty (P.map S.yield [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
toNull :: Monad m => Source m () Int -> m ()
toList :: Monad m => Source m () Int -> m [Int]
foldl :: Monad m => Source m () Int -> m Int
last :: Monad m => Source m () Int -> m (Maybe Int)

toNull src = S.runEffect $ S.for src S.discard
toList = S.toListM
foldl  = S.fold (+) 0 id
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Pipe m Int Int -> Source m () Int -> m ()
transform t src = S.runEffect $ S.for (src S.>-> t) S.discard

{-# INLINE composeN #-}
composeN :: Monad m => Int -> Pipe m Int Int -> Source m () Int -> m ()
composeN n f =
    case n of
        1 -> transform $ f
        2 -> transform $ f S.>-> f
        3 -> transform $ f S.>-> f S.>-> f
        4 -> transform $ f S.>-> f S.>-> f S.>-> f
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
    :: Monad m => Int -> Source m () Int -> m ()

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
    :: Monad m => Int -> Source m () Int -> m ()

scanMap    n = composeN n $ S.map (subtract 1) S.>-> S.scan (+) 0 id
dropMap    n = composeN n $ S.map (subtract 1) S.>-> S.drop 1
dropScan   n = composeN n $ S.scan (+) 0 id S.>-> S.drop 1
takeDrop   n = composeN n $ S.drop 1 S.>-> S.take maxValue
takeScan   n = composeN n $ S.scan (+) 0 id S.>-> S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) S.>-> S.take maxValue
filterDrop n = composeN n $ S.drop 1 S.>-> S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue S.>-> S.filter (<= maxValue)
filterScan n = composeN n $ S.scan (+) 0 id S.>-> S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) S.>-> S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Monad m => Source m () Int -> m ()
zip src = S.runEffect $ S.for (S.zip src src) S.discard

{-# INLINE concatMapFoldable #-}
concatMapFoldable :: Monad m => Source m () Int -> m ()
concatMapFoldable = transform (S.map (replicate 3) S.>-> S.concat)

main :: P.IO ()
main = $(defaultMain "Pipes")
