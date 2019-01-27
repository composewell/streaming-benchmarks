module ListPrelude
    ( module Data.List
    , module ListPrelude
    ) where

import Prelude (Maybe(..), id, otherwise)
import Data.List hiding (foldl', concatMap, last)
import qualified Data.List as L

type List = []

nil :: List a
nil = []

yield :: a -> List a
yield = (:[])

append :: List a -> List a -> List a
append = (++)

last :: List a -> Maybe a
last xs | null xs = Nothing
        | otherwise = Just (L.last xs)

toList :: List a -> [a]
toList = id

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = L.foldl'

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = L.concatMap

runStream :: List a -> List a
runStream = id

fromList :: [a] -> List a
fromList = id
