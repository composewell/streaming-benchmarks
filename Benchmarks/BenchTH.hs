{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.BenchTH
    ( mkBench
    , mkBenchN
    , purePackages
    , monadicPackages
    , monadicArrays
    , allPackages
    ) where

import Benchmarks.Common (benchIO, benchPure, benchIOArray)
import Language.Haskell.TH.Syntax (Q, Exp, mkName)
import Language.Haskell.TH.Lib (varE)

-- First item in the tuple is the module name and the second one is the
-- corresponding benchmark group name.
--
monadicPackages :: [(String, String)]
monadicPackages =
    [ ("Streamly", "streamly")
    , ("VectorStreams", "streams-vector")
    , ("Streaming", "streaming")
    , ("Machines", "machines")
    , ("Pipes", "pipes")
    , ("Conduit", "conduit")
    , ("Drinkery", "drinkery")
    ]

purePackages :: [(String, String)]
purePackages =
    [ -- stream like packages
      ("List", "list")
    , ("StreamlyPure", "pure-streamly")
    , ("DList", "dlist")
    , ("Sequence", "sequence")
    , ("ByteStringLazy", "lazy-bytestring")

    -- array like packages
    , ("ByteString", "bytestring")
    , ("Text", "text")
    , ("Vector", "vector")
    , ("VectorUnboxed", "unboxed-vector")
    , ("VectorStorable", "storable-vector")
    ]

monadicArrays :: [(String, String)]
monadicArrays = [("StreamlyArray", "array-streamly")]

allPackages :: [(String, String)]
allPackages =
       purePackages
    ++ monadicPackages
    ++ monadicArrays

-- mkBench <stream producer func> <stream consumer func> <module name>
-- <bench name>
mkBench :: String -> String -> String -> String -> Q Exp
mkBench f x mdl bname =
    case lookup mdl purePackages of
        Nothing -> case lookup mdl monadicPackages of
            Just _ ->
                [| benchIO bname $(varE (mkName f)) $(varE (mkName x)) |]
            Nothing ->
                if mdl == "StreamlyArray"
                then
                    [| benchIOArray "array-streamly"
                            $(varE (mkName f))
                            $(varE (mkName x))
                    |]
                else error $
                    "module " ++ show mdl ++ " not found in module list"
        Just _ ->
                [| benchPure bname $(varE (mkName f)) $(varE (mkName x)) |]

mkBenchN :: String -> String -> Int -> String -> String -> Q Exp
mkBenchN f x n mdl bname =
    case lookup mdl purePackages of
        Nothing -> case lookup mdl monadicPackages of
            Just _ ->
                [| benchIO bname $(varE (mkName f)) ($(varE (mkName x)) n) |]
            Nothing ->
                if mdl == "StreamlyArray"
                then
                    [| benchIOArray "array-streamly"
                            $(varE (mkName f))
                            ($(varE (mkName x)) n)
                    |]
                else
                    error $
                        "module " ++ show mdl ++ " not found in module list"
        Just _ ->
                [| benchPure bname $(varE (mkName f)) ($(varE (mkName x)) n)
                |]
