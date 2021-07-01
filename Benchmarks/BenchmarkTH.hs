{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Benchmarks.BenchmarkTH
    ( createBgroupSink
    , createBgroupSinkN
    , createBgroupSrc
    , pureMods
    , monadicMods
    , allMods
    , benchMods
    , iterMods
    , Select (..)
    ) where

import Data.List ((\\))
import Language.Haskell.TH.Syntax (Q, Exp, Lift)

import Benchmarks.BenchTH

benchMods, iterMods, pureMods, monadicMods, monadicArrayMods, allMods :: [String]
pureMods    = map fst purePackages
monadicMods = map fst monadicPackages
monadicArrayMods = map fst monadicArrays
allMods     = pureMods ++ monadicMods ++ monadicArrayMods
benchMods   = allMods \\ ["DList"]
iterMods = allMods \\
    [ "DList"
    , "Streaming"
    , "Machines"
    , "Pipes"
    , "Conduit"
    , "Drinkery"
    ]

data Select = Exclude [String] | Include [String] deriving Lift

-- | createBgroupSink <selection func> <module name> <benchmark name>
--                    <stream consumer function name>
createBgroupSink :: Select -> String -> String -> String -> Q Exp
createBgroupSink select modName bname fname =
    case select of
        Exclude mods -> f elem mods
        Include mods -> f notElem mods

    where

    f p mods =
        if p modName mods
        then [| Nothing |]
        else [| Just $(mkBench "source" fname modName bname) |]

-- | createBgroupSink <selection func> <module names> <benchmark name>
--                    <stream consumer function name> <number of iterations>
createBgroupSinkN :: Select -> String -> String -> String -> Int -> Q Exp
createBgroupSinkN select modName bname fname n =
    case select of
        Exclude mods -> f elem mods
        Include mods -> f notElem mods

    where

    f p mods =
        if p modName mods
        then [| Nothing |]
        else [| Just $(mkBenchN "source" fname n modName bname) |]

createBgroupSrc :: Select -> String -> String -> String -> Q Exp
createBgroupSrc select modName bname fname =
    case select of
        Exclude mods -> f elem mods
        Include mods -> f notElem mods

    where

    f p mods =
        if p modName mods
        then [| Nothing |]
        else [| Just $(mkBench fname "toNull" modName bname) |]
