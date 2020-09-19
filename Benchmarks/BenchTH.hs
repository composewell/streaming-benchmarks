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

    -- array like packages
    , ("ByteString", "bytestring")
    , ("ByteStringLazy", "lazy-bytestring")
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
mkBench :: String -> String -> String -> Q Exp
mkBench f x mdl =
    case lookup mdl purePackages of
        Nothing -> case lookup mdl monadicPackages of
            Just pkg ->
                [| benchIO pkg $(varE (mkName (mdl ++ "." ++ f)))
                               $(varE (mkName (mdl ++ "." ++ x)))
                |]
            Nothing ->
                if mdl == "StreamlyArray"
                then
                    [| benchIOArray "array-streamly"
                            $(varE (mkName (mdl ++ "." ++ f)))
                            $(varE (mkName (mdl ++ "." ++ x)))
                    |]
                else error $
                    "module " ++ show mdl ++ " not found in module list"
        Just pkg ->
                [| benchPure pkg $(varE (mkName (mdl ++ "." ++ f)))
                                 $(varE (mkName (mdl ++ "." ++ x)))
                |]

mkBenchN :: String -> String -> Int -> String -> Q Exp
mkBenchN f x n mdl =
    case lookup mdl purePackages of
        Nothing -> case lookup mdl monadicPackages of
            Just pkg ->
                [| benchIO pkg $(varE (mkName (mdl ++ "." ++ f)))
                               ($(varE (mkName (mdl ++ "." ++ x))) n)
                |]
            Nothing ->
                if mdl == "StreamlyArray"
                then
                    [| benchIOArray "array-streamly"
                            $(varE (mkName (mdl ++ "." ++ f)))
                            ($(varE (mkName (mdl ++ "." ++ x))) n)
                    |]
                else
                    error $
                        "module " ++ show mdl ++ " not found in module list"
        Just pkg ->
                [| benchPure pkg $(varE (mkName (mdl ++ "." ++ f)))
                               ($(varE (mkName (mdl ++ "." ++ x))) n)
                |]
