{-# LANGUAGE ConstraintKinds #-}

module Vector
    ( module Data.Vector.Fusion.Stream.Monadic
    , module Vector
    ) where

import Prelude hiding ((++), foldl', last, mapM_, mapM)
import Data.Vector.Fusion.Stream.Monadic
       hiding (enumFromStepN, last, mapM, unfoldrM)
import qualified Data.Vector.Fusion.Stream.Monadic as V

-- This way we don't need to add a MonadAsync constraint to this part
-- of the code.
type C = Monad

{-# INLINE unfoldrM #-}
unfoldrM :: (C m, Monad m) => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM = V.unfoldrM

{-# INLINE nil #-}
nil :: Monad m => Stream m a
nil = empty

{-# INLINE yield #-}
yield :: Monad m => a -> Stream m a
yield = singleton

{-# INLINE enumFromStepN #-}
enumFromStepN :: Monad m => Int -> Int -> Int -> Stream m Int
enumFromStepN = V.enumFromStepN

{-# INLINE mapM #-}
mapM :: (C m, Monad m) => (a -> m b) -> Stream m a -> Stream m b
mapM = V.mapM

{-# INLINE append #-}
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append = (++)

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = mapM_ (\_ -> return ())

{-# INLINE [1] last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = foldl' (\_ y -> Just y) Nothing
