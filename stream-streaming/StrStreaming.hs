{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}

module StrStreaming
    ( module StrStreaming
    ) where

import Control.Monad (void)
import Prelude hiding (mapM)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Data.Functor.Of

type C = Monad

newtype Stream m a = Stream
    { unStream :: S.Stream (Of a) m ()
    }

{-# INLINE unfoldrM #-}
unfoldrM :: (C m, Monad m) => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM step st = Stream (S.unfoldr gen st)
  where
    gen st = do
        nxt <- step st
        return $ case nxt of
            Nothing -> Left ()
            Just (a,s) -> Right (a,s)

{-# INLINE nil #-}
nil :: Monad m => Stream m a
nil = Stream (mempty)

{-# INLINE yield #-}
yield :: Monad m => a -> Stream m a
yield = Stream . S.yield

{-# INLINE replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n = Stream . S.replicate n

{-# INLINE fromList #-}
fromList :: Monad m => [a] -> Stream m a
fromList = Stream . S.each

{-# INLINE append #-}
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append m1 m2  = Stream ((unStream m1) >> (unStream m2))

{-# INLINE enumFromStepN #-}
enumFromStepN :: Monad m => Int -> Int -> Int -> Stream m Int
enumFromStepN x y n = Stream $ S.take n $ S.enumFromThen x y

-- Elimination
{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.effects . unStream

{-# INLINE toList #-}
toList :: Monad m => Stream m a -> m [a]
toList = S.toList_ . unStream

{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl' step acc = S.fold_ step acc id . unStream

{-# INLINE last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = S.last_ . unStream

-- Transformation = undefined
{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' step acc = Stream . S.scan step acc id . unStream

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f = Stream . S.map f . unStream

{-# INLINE mapM #-}
mapM :: (C m, Monad m) => (a -> m b) -> Stream m a -> Stream m b
mapM f = Stream . S.mapM f . unStream

{-# INLINE mapM_ #-}
mapM_ :: (Monad m) => (a -> m b) -> Stream m a -> m ()
mapM_ f = S.mapM_ f . unStream

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter pred = Stream . S.filter pred . unStream

{-# INLINE take #-}
take :: Monad m => Int -> Stream m a -> Stream m a
take n = Stream . S.take n . unStream

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile f = Stream . S.takeWhile f . unStream

{-# INLINE drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
drop n = Stream . S.drop n . unStream

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
dropWhile f = Stream . S.dropWhile f . unStream

-- Zipping
{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith zipper m1 m2 = Stream $ S.zipWith zipper (unStream m1) (unStream m2)

-- Concat
{-# INLINE concatMap #-}
concatMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap f m1 = Stream $ (S.for (unStream m1) (unStream . f))
