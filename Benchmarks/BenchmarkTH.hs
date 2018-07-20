{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.BenchmarkTH (createBgroup, createBgroupN) where

import Benchmarks.Common (benchIO, benchPure)
--import Benchmarks.Common (benchId)
import Language.Haskell.TH.Syntax (Q, Exp, mkName)
import Language.Haskell.TH.Lib (varE)

createBgroup :: String -> String -> Q Exp
createBgroup name fname =
    [|
        bgroup name
            [ benchIO "vector"    $(varE (mkName ("Vector.source")))
                                  $(varE (mkName ("Vector." ++ fname)))
            , benchIO "streamly"  $(varE (mkName ("Streamly.source")))
                                  $(varE (mkName ("Streamly." ++ fname)))
            , benchIO "streaming" $(varE (mkName ("Streaming.source")))
                                  $(varE (mkName ("Streaming." ++ fname)))
            , benchIO "machines"  $(varE (mkName ("Machines.source")))
                                  $(varE (mkName ("Machines." ++ fname)))
            , benchIO "pipes"     $(varE (mkName ("Pipes.source")))
                                  $(varE (mkName ("Pipes." ++ fname)))
            , benchIO "conduit"   $(varE (mkName ("Conduit.source")))
                                  $(varE (mkName ("Conduit." ++ fname)))
            , benchIO "drinkery"  $(varE (mkName ("Drinkery.source")))
                                  $(varE (mkName ("Drinkery." ++ fname)))
            , benchPure "list"    $(varE (mkName ("List.source")))
                                  $(varE (mkName ("List." ++ fname)))
            , benchPure "pure-vector" $(varE (mkName ("VectorPure.source")))
                                  $(varE (mkName ("VectorPure." ++ fname)))
            ]
    |]

createBgroupN :: String -> String -> Int -> Q Exp
createBgroupN name fname n =
    [|
        bgroup name
            [ benchIO "vector"    $(varE (mkName ("Vector.source")))
                                  ($(varE (mkName ("Vector." ++ fname))) n)
            , benchIO "streamly"  $(varE (mkName ("Streamly.source")))
                                  ($(varE (mkName ("Streamly." ++ fname))) n)
            , benchIO "streaming" $(varE (mkName ("Streaming.source")))
                                  ($(varE (mkName ("Streaming." ++ fname))) n)
            , benchIO "machines"  $(varE (mkName ("Machines.source")))
                                  ($(varE (mkName ("Machines." ++ fname))) n)
            , benchIO "pipes"     $(varE (mkName ("Pipes.source")))
                                  ($(varE (mkName ("Pipes." ++ fname))) n)
            , benchIO "conduit"   $(varE (mkName ("Conduit.source")))
                                  ($(varE (mkName ("Conduit." ++ fname))) n)
            , benchIO "drinkery"  $(varE (mkName ("Drinkery.source")))
                                  ($(varE (mkName ("Drinkery." ++ fname))) n)
            , benchPure "list"    $(varE (mkName ("List.source")))
                                  ($(varE (mkName ("List." ++ fname))) n)
            , benchPure "pure-vector" $(varE (mkName ("VectorPure.source")))
                                  ($(varE (mkName ("VectorPure." ++ fname))) n)
            ]
    |]
