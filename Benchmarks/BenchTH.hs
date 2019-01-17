{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.BenchTH
    ( mkBench
    , mkBenchN
    , purePackages
    , monadicPackages
    , allPackages
    ) where

import Benchmarks.Common (benchIO, benchPure)
import Language.Haskell.TH.Syntax (Q, Exp, mkName)
import Language.Haskell.TH.Lib (varE)

-- First item in the tuple is the module name and the second one is the
-- corresponding benchmark group name.
--
monadicPackages :: [(String, String)]
monadicPackages =
    [ ("Streamly", "streamly")
    , ("VectorMonadic", "monadic-vector")
    , ("Streaming", "streaming")
    , ("Machines", "machines")
    , ("Pipes", "pipes")
    , ("Conduit", "conduit")
    , ("Drinkery", "drinkery")
    ]

purePackages :: [(String, String)]
purePackages =
    [ ("List", "list")
    , ("DList", "dlist")
    , ("ByteString", "bytestring")
    , ("ByteStringLazy", "lazy-bytestring")
    , ("Text", "text")
    , ("Sequence", "sequence")
    , ("StreamlyPure", "pure-streamly")
    , ("Vector", "vector")
    ]

allPackages :: [(String, String)]
allPackages = purePackages ++ monadicPackages

mkBench :: String -> String -> String -> Q Exp
mkBench f x mdl =
    case lookup mdl purePackages of
        Nothing -> case lookup mdl monadicPackages of
            Just pkg ->
                [| benchIO pkg $(varE (mkName (mdl ++ "." ++ f)))
                               $(varE (mkName (mdl ++ "." ++ x)))
                |]
            Nothing -> error $
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
            Nothing -> error $
                "module " ++ show mdl ++ " not found in module list"
        Just pkg ->
                [| benchPure pkg $(varE (mkName (mdl ++ "." ++ f)))
                               ($(varE (mkName (mdl ++ "." ++ x))) n)
                |]
