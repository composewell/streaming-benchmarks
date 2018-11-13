{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.BenchmarkTH
    ( createBgroupSink
    , createBgroupSinkN
    , createBgroupSrc
    , pureMods
    , monadicMods
    , allMods
    , benchMods
    , iterMods
    ) where

import Data.List ((\\))
import Language.Haskell.TH.Syntax (Q, Exp)
import Language.Haskell.TH.Lib (listE)

import Benchmarks.BenchTH

benchMods, iterMods, pureMods, monadicMods, allMods :: [String]
pureMods    = map fst purePackages
monadicMods = map fst monadicPackages
allMods     = pureMods ++ monadicMods
benchMods   = allMods \\ ["DList"]
iterMods = allMods \\
    [ "DList"
    , "Streaming"
    , "Machines"
    , "Pipes"
    , "Conduit"
    , "Drinkery"
    ]

createBgroupSink :: [String] -> String -> String -> Q Exp
createBgroupSink mods name fname =
    [|
        bgroup name $(listE (map (mkBench "source" fname) mods))
    |]

createBgroupSinkN :: [String] -> String -> String -> Int -> Q Exp
createBgroupSinkN mods name fname n =
    [|
        bgroup name $(listE (map (mkBenchN "source" fname n) mods))
    |]

createBgroupSrc :: [String] -> String -> String -> Q Exp
createBgroupSrc mods name fname =
    [|
        bgroup name $(listE (map (mkBench fname "toNull") mods))
    |]
