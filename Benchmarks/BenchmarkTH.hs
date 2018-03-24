{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.BenchmarkTH (createBgroup, createScaling) where

import Benchmarks.Common (benchIO)
import Language.Haskell.TH.Syntax (Q, Exp, mkName)
import Language.Haskell.TH.Lib (varE)

createBgroup :: String -> String -> Q Exp
createBgroup name fname =
    [|
        bgroup name
            [ benchIO "vector" $(varE (mkName ("Vector." ++ fname))) n
            , benchIO "streamly" $(varE (mkName ("Streamly." ++ fname))) n
            , benchIO "streaming" $(varE (mkName ("Streaming." ++ fname))) n
            , benchIO "machines" $(varE (mkName ("Machines." ++ fname))) n
            , benchIO "pipes" $(varE (mkName ("Pipes." ++ fname))) n
            , benchIO "conduit" $(varE (mkName ("Conduit." ++ fname))) n
            ]
    |]

createScaling :: String -> String -> Q Exp
createScaling name mname =
    [|
        bgroup name
            [ benchIO "1" ($(varE (mkName (mname ++ ".composeScaling"))) 1) n
            , benchIO "2" ($(varE (mkName (mname ++ ".composeScaling"))) 2) n
            , benchIO "3" ($(varE (mkName (mname ++ ".composeScaling"))) 3) n
            , benchIO "4" ($(varE (mkName (mname ++ ".composeScaling"))) 4) n
            ]
    |]
