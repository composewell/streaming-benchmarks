module Main (main) where

import BenchRunner (mainWith)
import BuildLib (Quickness(..))
import Control.Applicative ((<|>))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)

-- tags with _grp and _cmp suffixes are special
 -- [(<benchmark name>", [<tags>])]
targets :: [(String, [String])]
targets =
    [ ("StreamlyPure", ["pure_stream_cmp"])
    , ("List", ["pure_stream_cmp"])
    , ("Streamly", ["io_stream_cmp"])
    , ("ByteStringLazy", ["io_stream_cmp"])
    , ("VectorStreams", [])
    -- , ("Streaming", [])
    -- , ("Machines", [])
    -- , ("Pipes", [])
    -- , ("Conduit", [])
    -- , ("Drinkery", [])
    ]

rtsOpts :: String -> String -> String
rtsOpts exeName benchName0 = unwords [general, exeSpecific, benchSpecific]

    where

    -- Drop All.
    benchName = drop 4 benchName0
    general
        -- | "o-1-sp" `isInfixOf` benchName = "-K36K -M16M"
        | otherwise = ""
    exeSpecific
        -- | "Prelude.Concurrent" `isSuffixOf` exeName = "-K512K -M384M"
        | otherwise = ""
    benchSpecific
        -- | "Data.Stream.StreamD/o-n-space.elimination.toList" == benchName = "-K2M"
        | otherwise = ""

speedOpts :: String -> String -> Maybe Quickness
speedOpts exeName benchName0 = exeSpecific <|> benchSpecific

    where

    -- slowestOf Quicker _ = Quicker
    -- slowestOf _ Quicker = Quicker
    -- slowestOf _ _ = SuperQuick

    -- Drop All.
    benchName = drop 4 benchName0
    exeSpecific
        -- | "Prelude.Concurrent" == exeName = Just SuperQuick
        | otherwise = Nothing
    benchSpecific
        -- | "Prelude.Parallel/o-n-heap.mapping.mapM" == benchName = Just SuperQuick
        | otherwise = Nothing

main :: IO ()
main = mainWith targets speedOpts rtsOpts
