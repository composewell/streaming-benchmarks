{-# LANGUAGE ConstraintKinds #-}

module ListVector
    ( module Data.Vector
    , module ListVector
    ) where

import Prelude hiding ((++), foldl', last, mapM_, mapM)
import Data.Vector
       hiding (last, mapM, unfoldrM)
import qualified Data.Vector as V

type List = Vector

{-# INLINE nil #-}
nil :: List a
nil = empty

{-# INLINE yield #-}
yield :: a -> List a
yield = singleton

{-# INLINE append #-}
append :: List a -> List a -> List a
append = (++)

{-# INLINE runStream #-}
runStream :: List a -> List a
runStream = id

{-# INLINE [1] last #-}
last ::  List a -> Maybe a
last xs | V.null xs = Nothing
        | otherwise = Just (V.last xs)
