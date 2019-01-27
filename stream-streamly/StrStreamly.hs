{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module StrStreamly
    ( module Streamly
    , module StrStreamly
    ) where

import Prelude hiding (mapM)
import Streamly
import qualified Streamly.Prelude as S

type Stream = SerialT

class MonadAsync m => C m
instance MonadAsync m => C m

{-# INLINE unfoldrM #-}
unfoldrM :: (C m, Monad m) => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM = S.unfoldrM

{-# INLINE mapM #-}
mapM :: (C m, Monad m) => (a -> m b) -> Stream m a -> Stream m b
mapM = S.mapM

{-# INLINE append #-}
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append = (<>)

{-# INLINE enumFromStepN #-}
enumFromStepN :: Monad m => Int -> Int -> Int -> Stream m Int
enumFromStepN = S.enumerateFromThenTo

{-# INLINE nil #-}
nil :: Monad m => Stream m a
nil = S.nil

{-# INLINE yield #-}
yield :: Monad m => a -> Stream m a
yield = S.yield

{-# INLINE replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate = S.replicate

{-# INLINE fromList #-}
fromList :: Monad m => [a] -> Stream m a
fromList = S.fromList

-- Elimination

{-# INLINE toList #-}
toList :: Monad m => Stream m a -> m [a]
toList = S.toList

{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl' = S.foldl'

{-# INLINE last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = S.last

-- Transformation

{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' = S.scanl'

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map = S.map

{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM_ = S.mapM_

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter = S.filter

{-# INLINE take #-}
take :: Monad m => Int -> Stream m a -> Stream m a
take = S.take

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile = S.takeWhile

{-# INLINE drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
drop = S.drop

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
dropWhile = S.dropWhile

-- Zipping

{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith = S.zipWith

-- Concat

{-# INLINE concatMap #-}
concatMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap = S.concatMap
