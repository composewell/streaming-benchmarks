-- |
-- Module      : Benchmarks.Conduit
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.Conduit where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Prelude
       (Monad, Int, (+), ($), return, even, (>), (<=),
        subtract, undefined, replicate, (<$>), (<*>), Maybe(..), foldMap, (.),
        maxBound)
import qualified Prelude as P
import Data.Semigroup ((<>))

import qualified Data.Conduit as S
import qualified Data.Conduit.Combinators as S
import qualified Data.Conduit.List as SL
-- import Data.Conduit.List (sourceList)

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m i a = S.ConduitT i a m ()
type Sink   m a r = S.ConduitT a S.Void m r
type Pipe   m a b = S.ConduitT a b m ()

{-# INLINE source #-}
source :: Monad m => Int -> Source m () Int
-- source n = sourceList [n..n+value]
source n = SL.unfoldM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Monad m => Int -> Source m () Int
appendSourceR n = foldMap (S.yieldM . return) [n..n+appendValue]

{-# INLINE appendSourceL #-}
appendSourceL :: Monad m => Int -> Source m () Int
appendSourceL n =
    P.foldl (<>) P.mempty (P.map (S.yieldM . return) [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- eval :: Monad m => Sink m a ()
-- eval = S.foldl (\_ x -> P.seq x ()) ()

{-# INLINE runStream #-}
-- runStream :: Monad m => Sink m Int () -> Source m () Int -> m ()
-- runStream t src = S.runConduit $ src S..| t S..| S.foldl (\_ x -> P.seq x ()) ()
runStream :: Monad m => Sink m Int a -> Source m () Int -> m a
runStream t src = S.runConduit $ src S..| t

{-# INLINE toNull #-}
toNull :: Monad m => Source m () Int -> m ()
toNull = runStream $ S.sinkNull
-- toNull src = S.runConduit $ src S..| eval

{-# INLINE toList #-}
toList :: Monad m => Source m () Int -> m [Int]
toList = runStream $ S.sinkList

{-# INLINE foldl #-}
foldl :: Monad m => Source m () Int -> m Int
foldl  = runStream $ S.foldl (+) 0

{-# INLINE last #-}
last :: Monad m => Source m () Int -> m (Maybe Int)
last   = runStream $ S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Pipe m Int Int -> Source m () Int -> m ()
-- mapM_ is much more costly compared to sinkNull
-- transform t = runStream (t S..| S.mapM_ (\_ -> return ()))
-- transform t s = runStream (t S..| eval) s P.>>= \x -> P.seq x (return ())
transform t = runStream (t S..| S.sinkNull)

{-# INLINE composeN #-}
composeN :: Monad m => Int -> Pipe m Int Int -> Source m () Int -> m ()
composeN n f =
    case n of
        1 -> transform $ f
        2 -> transform $ f S..| f
        3 -> transform $ f S..| f S..| f
        4 -> transform $ f S..| f S..| f S..| f
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

scan           n = composeN n $ S.scanl (+) 0
map            n = composeN n $ S.map (+1)
mapM           n = composeN n $ S.mapM return
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxValue)
filterAllIn    n = composeN n $ S.filter (<= maxValue)
takeOne        n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take maxValue
takeWhileTrue  n = composeN n $ S.takeWhile (<= maxValue)
dropOne        n = composeN n $ (S.drop 1 P.>> S.awaitForever S.yield)
dropAll        n = composeN n $ (S.drop maxValue P.>> S.awaitForever S.yield)
dropWhileFalse n = composeN n $ (S.dropWhile (> maxValue) P.>>
                            S.awaitForever S.yield)
dropWhileTrue  n = composeN n $ (S.dropWhile (<= maxValue) P.>>
                            S.awaitForever S.yield)

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

scanMap    n = composeN n $ S.map (subtract 1) S..| S.scanl (+) 0
dropMap    n = composeN n $ S.map (subtract 1) S..|
                (S.drop 1 P.>> S.awaitForever S.yield)
dropScan   n = composeN n $ S.scanl (+) 0 S..|
                (S.drop 1 P.>> S.awaitForever S.yield)
takeDrop   n = composeN n $ (S.drop 1 P.>> S.awaitForever S.yield) S..|
                S.take maxValue
takeScan   n = composeN n $ S.scanl (+) 0 S..| S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) S..| S.take maxValue
filterDrop n = composeN n $ (S.drop 1 P.>> S.awaitForever S.yield) S..|
                S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue S..| S.filter (<= maxValue)
filterScan n = composeN n $ S.scanl (+) 0 S..| S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) S..| S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Monad m => Source m () Int -> m ()

zip src = S.runConduit $
        (   S.getZipSource $ (,)
        <$> S.ZipSource src
        <*> S.ZipSource src) S..| S.sinkNull -- eval

{-# INLINE concatMapFoldable #-}
concatMapFoldable :: Monad m => Source m () Int -> m ()
concatMapFoldable = transform (S.map (replicate 3) S..| S.concat)

main :: P.IO ()
main = $(defaultMain "Conduit")
