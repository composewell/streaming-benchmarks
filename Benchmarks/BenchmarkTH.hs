{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.BenchmarkTH (createBgroup, createScaling) where

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

createScaling :: String -> String -> Q Exp
createScaling name mname =
    [| let src = $(varE (mkName (mname ++ ".source")))
       in  bgroup name
            [ benchIO "1" src ($(varE (mkName (mname ++ ".composeScaling"))) 1)
            , benchIO "2" src ($(varE (mkName (mname ++ ".composeScaling"))) 2)
            , benchIO "3" src ($(varE (mkName (mname ++ ".composeScaling"))) 3)
            , benchIO "4" src ($(varE (mkName (mname ++ ".composeScaling"))) 4)
            ]
    |]
