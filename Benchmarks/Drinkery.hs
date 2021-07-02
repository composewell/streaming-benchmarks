{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.Drinkery where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue)
import Control.Monad (void)
import Prelude
       (Monad, Int, Maybe(..), (+), ($), return, even, (>), (<=),
        subtract, undefined, replicate, (<$>), (<*>), fst, id, const,
        maxBound, IO)

import qualified Data.Drinkery as S
import qualified Data.Drinkery.Finite as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m i o = S.Source () o m
type Pipe   m i o = S.Pipe i o m
type Sink   m a r = S.Sink (S.Source () a) m r

{-# INLINE source #-}
source :: Monad m => Int -> Source m () Int
source n = S.unfoldrTapM
  (const $ \x -> return (if x > n + value then Nothing else Just x, x + 1)) n

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Pipe m Int o -> Source m () Int -> m ()
runStream t src = void $ src S.++& t S.$& S.drainFrom S.consume

{-# INLINE eliminate #-}
eliminate :: Monad m => Sink m Int a -> Source m () Int -> m ()
eliminate s src = void $ src S.++& s

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
toNull, toList, foldl, last :: Monad m => Source m () Int -> m ()
toNull = eliminate $ S.drainFrom S.consume
toList = eliminate S.drinkUp
foldl  = eliminate $ S.foldlFrom' S.consume (+) 0
last   = eliminate $ S.lastFrom S.consume

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Pipe m Int o -> Source m () Int -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int
    -> (forall n. Monad n => Pipe n Int Int)
    -> Source m () Int
    -> m ()
composeN n f =
    case n of
        1 -> transform $ f
        2 -> transform $ f S.++$ f
        3 -> transform $ f S.++$ f S.++$ f
        4 -> transform $ f S.++$ f S.++$ f S.++$ f
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

scan           n = composeN n $ S.scan (+) 0
map            n = composeN n $ S.map (+1)
mapM           n = composeN n $ S.traverse return
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

scanMap    n = composeN n $ S.map (subtract 1) S.++$ S.scan (+) 0
dropMap    n = composeN n $ S.map (subtract 1) S.++$ S.drop 1
dropScan   n = composeN n $ S.scan (+) 0 S.++$ S.drop 1
takeDrop   n = composeN n $ S.drop 1 S.++$ S.take maxValue
takeScan   n = composeN n $ S.scan (+) 0 S.++$ S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) S.++$ S.take maxValue
filterDrop n = composeN n $ S.drop 1 S.++$ S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue S.++$ S.filter (<= maxValue)
filterScan n = composeN n $ S.scan (+) 0 S.++$ S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) S.++$ S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Monad m => Source m () Int -> m ()

zip src = void
  $ S.unJoint ((,) <$> S.Joint src <*> S.Joint src)
  S.++& S.drainFrom (fst <$> S.consume)

{-# INLINE concatMapFoldable #-}
concatMapFoldable :: Monad m => Source m () Int -> m ()
concatMapFoldable = transform $ S.map (replicate 3) S.++$ S.concatMap id

main :: IO ()
main = $(defaultMain "Drinkery")
