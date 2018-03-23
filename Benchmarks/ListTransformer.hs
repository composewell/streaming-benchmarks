-- |
-- Module      : Benchmarks.ListTransformer
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

module Benchmarks.ListTransformer where

import Benchmarks.Common (value, maxValue)
import Control.Monad (void, (>=>))
import Control.Monad.Trans.Class (lift)
import Prelude
       (Monad, Int, (+), id, ($), (.), return, fmap, even, (>), (<=),
        (>>=), subtract, undefined)

import qualified List.Transformer as S

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

type Stream m a = S.ListT m a

source :: Monad m => Int -> Stream m Int
source n = S.select [n..n+value]

runStream :: Monad m => Stream m a -> m ()
runStream = S.runListT

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

eliminate :: Monad m => (Stream m Int -> m a) -> Int -> m ()
eliminate f = void . f . source

toNull = eliminate $ runStream
toList = eliminate $ undefined
foldl  = eliminate $ S.fold (+) 0 id
last   = eliminate $ undefined

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

transform :: Monad m => (Int -> Stream m Int) -> Int -> m ()
transform f n = runStream (source n >>= f)

scan          = transform $ undefined
map           = transform $ (lift . return . (+1))
mapM          = transform $ undefined
filterEven    = transform $ undefined
filterAllOut  = transform $ undefined
filterAllIn   = transform $ undefined
takeOne       = transform $ undefined
takeAll       = transform $ undefined
takeWhileTrue = transform $ undefined
dropAll       = transform $ undefined
dropWhileTrue = transform $ undefined

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip n         = undefined
concat _n     = undefined

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

compose :: Monad m => (Int -> Stream m Int) -> Int -> m ()
compose f = transform $ (f >=> f >=> f >=> f)

composeMapM           = compose (lift . return)
composeAllInFilters   = compose undefined
composeAllOutFilters  = compose undefined
composeMapAllInFilter = compose undefined

composeScaling :: Monad m => Int -> Int -> m ()
composeScaling m n =
    case m of
        1 -> transform f n
        2 -> transform (f >=> f) n
        3 -> transform (f >=> f >=> f) n
        4 -> transform (f >=> f >=> f >=> f) n
        _ -> undefined
    where f = undefined
