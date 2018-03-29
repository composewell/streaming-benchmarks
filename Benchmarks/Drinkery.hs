{-# LANGUAGE RankNTypes #-}
module Benchmarks.Drinkery where

import Benchmarks.Common (value, maxValue)
import Control.Monad (void)
import Prelude
       (Monad, Int, (+), ($), return, even, (>), (<=),
        subtract, undefined, replicate, (<$>), (<*>), fst, id)

import qualified Data.Drinkery as S
import qualified Data.Drinkery.Finite as S

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
toNull, toList, foldl, last, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Int -> m ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m i o = S.Source () o m
type Pipe   m i o = S.Pipe i o m
type Sink   m a r = S.Sink (S.Source () a) m r

{-# INLINE source #-}
source :: Monad m => Int -> Source m () Int
source n = S.tapListT $ S.sample [n .. n + value]

{-# INLINE runStream #-}
runStream :: Monad m => Pipe m Int o -> Int -> m ()
runStream t n = void $ source n S.++& t S.$& S.drainFrom S.consume

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE eliminate #-}
eliminate :: Monad m => Sink m Int a -> Int -> m ()
eliminate s n = void $ source n S.++& s

toNull = eliminate $ S.drainFrom S.consume
toList = eliminate S.drinkUp
foldl  = eliminate $ S.foldlFrom' S.consume (+) 0
last   = eliminate $ S.lastFrom S.consume

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Pipe m Int o -> Int -> m ()
transform = runStream

scan          = transform $ S.scan (+) 0
map           = transform $ S.map (+1)
mapM          = transform $ S.traverse return
filterEven    = transform $ S.filter even
filterAllOut  = transform $ S.filter (> maxValue)
filterAllIn   = transform $ S.filter (<= maxValue)
takeOne       = transform $ S.take 1
takeAll       = transform $ S.take maxValue
takeWhileTrue = transform $ S.takeWhile (<= maxValue)
dropAll       = transform $ S.drop maxValue
dropWhileTrue = transform $ S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip n = void
  $ S.unJoint ((,) <$> S.Joint (source n) <*> S.Joint (source n))
  S.++& S.drainFrom (fst <$> S.consume)
concat = transform $ S.map (replicate 3) S.++$ S.concatMap id

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: Monad m => (forall n. Monad n => Pipe n Int Int) -> Int -> m ()
compose f = transform (f S.++$ f S.++$ f S.++$ f)

composeMapM           = compose (S.traverse return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.map (subtract 1) S.++$ S.filter (<= maxValue))

composeScaling :: Monad m => Int -> Int -> m ()
composeScaling m n =
    case m of
        1 -> transform f n
        2 -> transform (f S.++$ f) n
        3 -> transform (f S.++$ f S.++$ f) n
        4 -> transform (f S.++$ f S.++$ f S.++$ f) n
        _ -> undefined
    where f :: Monad m => Pipe m Int Int
          f = S.filter (<= maxValue)
