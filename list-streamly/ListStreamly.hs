module ListStreamly
    ( module Streamly
    , module ListStreamly
    ) where

import Data.Functor.Identity

import Prelude hiding (map)
import Streamly hiding (runStream)
import qualified Streamly.Prelude as S

type List = SerialT Identity

{-# INLINE unfoldr #-}
unfoldr :: (s -> Maybe (a, s)) -> s -> List a
unfoldr = S.unfoldr

{-# INLINE nil #-}
nil :: List a
nil = S.nil

{-# INLINE yield #-}
yield :: a -> List a
yield = S.yield

{-# INLINE replicate #-}
replicate :: Int -> a -> List a
replicate = S.replicate

{-# INLINE fromList #-}
fromList :: [a] -> List a
fromList = S.fromList

{-# INLINE append #-}
append :: List a -> List a -> List a
append = (<>)

-- Elimination

{-# INLINE runStream #-}
runStream :: List a -> List a
runStream = id

{-# INLINE toList #-}
toList :: List a -> [a]
toList = runIdentity . S.toList

{-# INLINE foldl' #-}
foldl' :: (b -> a -> b) -> b -> List a -> b
foldl' step acc = runIdentity . S.foldl' step acc

{-# INLINE last #-}
last :: List a -> Maybe a
last = runIdentity . S.last

-- Transformation

{-# INLINE scanl' #-}
scanl' :: (b -> a -> b) -> b -> List a -> List b
scanl' = S.scanl'

{-# INLINE map #-}
map :: (a -> b) -> List a -> List b
map = S.map

{-# INLINE filter #-}
filter :: (a -> Bool) -> List a -> List a
filter = S.filter

{-# INLINE take #-}
take :: Int -> List a -> List a
take = S.take

{-# INLINE takeWhile #-}
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile = S.takeWhile

{-# INLINE drop #-}
drop :: Int -> List a -> List a
drop = S.drop

{-# INLINE dropWhile #-}
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile = S.dropWhile

-- Zipping

{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith = S.zipWith

-- Concat

{-# INLINE concatMap #-}
concatMap :: (a -> List b) -> List a -> List b
concatMap = S.concatMap
