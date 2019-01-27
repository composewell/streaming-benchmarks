module ListSequence
    ( module Data.Sequence
    , module ListSequence
    ) where

import Prelude
       hiding (concatMap, dropWhile, null, scanl, takeWhile)
import Data.Sequence
import qualified Data.Foldable as F

type List = Seq

nil :: List a
nil = empty

yield :: a -> List a
yield = singleton

append :: List a -> List a -> List a
append = (><)

map :: (a -> b) -> List a -> List b
map = fmap

concatMap :: (a -> List b) -> List a -> List b
concatMap = flip (>>=)

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile = takeWhileL

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile = dropWhileL

-- Assuming scanl' == scanl, since Seq is strict.
scanl' :: (b -> a -> b) -> b -> List a -> List b
scanl' = scanl

runStream :: List a -> List a
runStream = id

toList :: Seq a -> [a]
toList = F.toList

foldl' :: (b -> a -> b) -> b -> List a -> b
foldl' = F.foldl'

last :: List a -> Maybe a
last xs =
    case viewr xs of
        EmptyR -> Nothing
        _ :> a -> Just a
