{-# LANGUAGE FlexibleContexts          #-}
module Main (main) where

import Control.Monad.Identity
import Gauge.Main
import Data.Function ((&))

import qualified Data.Conduit      as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as C
import qualified Pipes             as P
import qualified Pipes.Prelude     as P
import qualified Streaming.Prelude as S

value :: Int
value = 1000000

drainP :: P.Proxy () Int () a Identity () -> ()
drainP p = runIdentity $ P.runEffect $ P.for (sourceP P.>-> p) P.discard

drainC :: C.Conduit Int Identity a -> ()
drainC c = runIdentity $ (sourceC C.$= c) C.$$ CC.sinkNull

drainS :: (S.Stream (S.Of Int) Identity () -> S.Stream (S.Of Int) Identity ())
    -> ()
drainS s = runIdentity $ S.effects $ sourceS & s

sourceC :: Monad m => C.Producer m Int
sourceC = C.enumFromTo 1 value

sourceP :: Monad m => P.Producer' Int m ()
sourceP = P.each [1..value]

sourceS :: Monad m => S.Stream (S.Of Int) m ()
sourceS = S.each [1..value]

main :: IO ()
main =
  defaultMain
          [ bench "streaming" $ whnf drainS (S.map (+1))
          , bench "pipes" $ whnf drainP (P.map (+1))
          , bench "conduit" $ whnf drainC (C.map (+1))
          ]
