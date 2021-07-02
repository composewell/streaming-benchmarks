-- |
-- Module      : Benchmarks.SimpleConduit
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.SimpleConduit where

import Benchmarks.DefaultMain (defaultMain)
import Benchmarks.Common (value, maxValue)
import Control.Monad (void)
import Prelude
       (Monad, Int, (+), id, ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined)

import qualified Conduit.Simple as S

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

toNull, toList, foldl, last, scan, map, filterEven, mapM, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeMapM, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Int -> m ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.StreamT m a

source :: Int -> Stream m Int
source n = S.each [n..n+value]

runStream :: Monad m => Stream m a -> m ()
runStream = S.runStreamT

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

eliminate :: Monad m => (Stream m Int -> m a) -> Int -> m ()
eliminate f = void . f . source

toNull = eliminate $ runStream
toList = eliminate $ S.toList
foldl  = eliminate $ S.foldl (+) 0 id
last   = eliminate $ S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

transform :: Monad m => (Stream m Int -> Stream m a) -> Int -> m ()
transform f = runStream . f . source

scan          = transform $ S.scan (+) 0 id
map           = transform $ fmap (+1)
mapM          = transform $ S.mapM return
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

zip n         = runStream $ (S.zipWith (,) (source n) (source n))
concat _n     = undefined

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

compose :: Monad m => (Stream m Int -> Stream m Int) -> Int -> m ()
compose f = transform $ (f . f . f . f)

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.filter (<= maxValue) . fmap (subtract 1))

composeScaling :: Monad m => Int -> Int -> m ()
composeScaling m n =
    case m of
        1 -> transform f n
        2 -> transform (f . f) n
        3 -> transform (f . f . f) n
        4 -> transform (f . f . f . f) n
        _ -> undefined
    where f = S.filter (<= maxValue)

main :: P.IO ()
main = $(defaultMain "SimpleConduit")
