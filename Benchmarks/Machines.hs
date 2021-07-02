-- |
-- Module      : Benchmarks.Machines
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Benchmarks.Machines where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue, appendValue)
import Prelude
       (Monad, Int, (.), (+), ($), return, even, (>), (<=),
        subtract, replicate, Maybe(..), maxBound, foldMap)
import qualified Prelude as P
import Data.Semigroup ((<>))

import qualified Data.Machine      as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Source m o = S.SourceT m o
type Pipe   m i o = S.ProcessT m i o

source :: Monad m => Int -> Source m Int
-- source n = S.source [n..n+value]
source n = S.unfoldT step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE appendSourceR #-}
appendSourceR :: Monad m => Int -> Source m Int
appendSourceR n = foldMap (S.construct . S.yield) [n..n+appendValue]

-- XXX use S.prepended instead?
{-# INLINE appendSourceL #-}
appendSourceL :: Monad m => Int -> Source m Int
appendSourceL n = P.foldl (<>) P.mempty (P.map (S.construct . S.yield) [n..n+appendValue])

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Pipe m Int o -> S.MachineT m k Int -> m ()
runStream t src = S.runT_ $ src S.~> t

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
toNull, foldl, last :: Monad m => S.MachineT m k Int -> m ()
toList :: Monad m => S.MachineT m k Int -> m [Int]

toNull = S.runT_
toList = S.runT
foldl  = runStream $ S.fold (+) 0
last   = runStream $ S.final

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Pipe m Int o -> S.MachineT m k Int -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN :: Monad m => Int -> Pipe m Int Int -> S.MachineT m k Int -> m ()
composeN n f =
    case n of
        1 -> transform $ f
        2 -> transform $ f S.~> f
        3 -> transform $ f S.~> f S.~> f
        4 -> transform $ f S.~> f S.~> f S.~> f
        -- _ -> undefined

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
    :: Monad m => Int -> S.MachineT m k Int -> m ()

scan           n = composeN n $ S.scan (+) 0
map            n = composeN n $ S.mapping (+1)
mapM           n = composeN n $ S.autoM return
filterEven     n = composeN n $ S.filtered even
filterAllOut   n = composeN n $ S.filtered (> maxValue)
filterAllIn    n = composeN n $ S.filtered (<= maxValue)
takeOne        n = composeN n $ S.taking 1
takeAll        n = composeN n $ S.taking maxValue
takeWhileTrue  n = composeN n $ S.takingWhile (<= maxValue)
dropOne        n = composeN n $ S.dropping 1
dropAll        n = composeN n $ S.dropping maxValue
dropWhileFalse n = composeN n $ S.droppingWhile (> maxValue)
dropWhileTrue  n = composeN n $ S.droppingWhile (<= maxValue)

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
    :: Monad m => Int -> S.MachineT m k Int -> m ()

scanMap    n = composeN n $ S.mapping (subtract 1) S.~> S.scan (+) 0
dropMap    n = composeN n $ S.mapping (subtract 1) S.~> S.dropping 1
dropScan   n = composeN n $ S.scan (+) 0 S.~> S.dropping 1
takeDrop   n = composeN n $ S.dropping 1 S.~> S.taking maxValue
takeScan   n = composeN n $ S.scan (+) 0 S.~> S.taking maxValue
takeMap    n = composeN n $ S.mapping (subtract 1) S.~> S.taking maxValue
filterDrop n = composeN n $ S.dropping 1 S.~> S.filtered (<= maxValue)
filterTake n = composeN n $ S.taking maxValue S.~> S.filtered (<= maxValue)
filterScan n = composeN n $ S.scan (+) 0 S.~> S.filtered (<= maxBound)
filterMap  n = composeN n $ S.mapping (subtract 1) S.~> S.filtered (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Monad m => S.MachineT m k Int -> m ()

zip _src = S.runT_ (S.capT (source 10) (source 20) S.zipping)

{-# INLINE concatMapFoldable #-}
concatMapFoldable :: Monad m => S.MachineT m k Int -> m ()
concatMapFoldable = transform (S.mapping (replicate 3) S.~> S.asParts)

main :: P.IO ()
main = $(defaultMain "Machines")
