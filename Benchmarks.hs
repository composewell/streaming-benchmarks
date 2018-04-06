-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Benchmarks.BenchmarkTH (createBgroup, createScaling)

import qualified Benchmarks.Vector as Vector
import qualified Benchmarks.Streamly as Streamly
import qualified Benchmarks.Streaming as Streaming
import qualified Benchmarks.Machines as Machines
import qualified Benchmarks.Pipes as Pipes
import qualified Benchmarks.Conduit as Conduit
import qualified Benchmarks.Drinkery as Drinkery
-- import qualified Benchmarks.LogicT as LogicT
-- import qualified Benchmarks.ListT as ListT
-- import qualified Benchmarks.ListTransformer as ListTransformer

import Gauge

main :: IO ()
main = do
  defaultMain
    [ bgroup "elimination"
      [ $(createBgroup "toNull" "toNull")
      , $(createBgroup "toList" "toList")
      , $(createBgroup "fold" "foldl")
      , $(createBgroup "last" "last")
      ]
    , bgroup "transformation"
      [ $(createBgroup "scan" "scan")
      , $(createBgroup "map" "map")
      , $(createBgroup "mapM" "mapM")
      , $(createBgroup "concat" "concat")
      ]
    , bgroup "filtering"
      [ $(createBgroup "filter-even" "filterEven")
      , $(createBgroup "filter-all-out" "filterAllOut")
      , $(createBgroup "filter-all-in" "filterAllIn")
      , $(createBgroup "take-all" "takeAll")
      , $(createBgroup "takeWhile-true" "takeWhileTrue")
      , $(createBgroup "drop-all" "dropAll")
      , $(createBgroup "dropWhile-true" "dropWhileTrue")
      ]
    , $(createBgroup "zip" "zip")
    , bgroup "compose"
      [ $(createBgroup "mapM" "composeMapM")
      , $(createBgroup "map-with-all-in-filter" "composeMapAllInFilter")
      , $(createBgroup "all-in-filters" "composeAllInFilters")
      , $(createBgroup "all-out-filters" "composeAllOutFilters")
      ]
    -- XXX Disabling this for now to reduce the running time
    -- We need a way to include/exclude this dynamically
    {-
    , bgroup "compose-scaling"
        -- Scaling with same operation in sequence
      [ $(createScaling "vector-filters" "Vector")
      , $(createScaling "streamly-filters" "Streamly")
      , $(createScaling "streaming-filters" "Streaming")
      , $(createScaling "machines-filters" "Machines")
      , $(createScaling "pipes-filters" "Pipes")
      , $(createScaling "conduit-filters" "Conduit")
      ]
      -}
   ]
