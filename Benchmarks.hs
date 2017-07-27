module Main (main) where

import Control.Monad (void)
import Control.Monad.Identity
import Criterion.Main
import Data.Function ((&))
import System.Random (randomIO)

import qualified Data.Conduit      as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as C
import qualified Data.Machine      as M
import qualified Pipes             as P
import qualified Pipes.Prelude     as P
import qualified Streaming.Prelude as S

value :: Int
value = 1000000

drainM :: M.ProcessT Identity Int o -> ()
drainM m = runIdentity $ M.runT_ (sourceM M.~> m)

drainMIO :: M.ProcessT IO Int o -> IO ()
drainMIO m = M.runT_ (sourceM M.~> m)

drainP :: P.Proxy () Int () a Identity () -> ()
drainP p = runIdentity $ P.runEffect $ P.for (sourceP P.>-> p) P.discard

drainPIO :: P.Proxy () Int () a IO () -> IO ()
drainPIO p = P.runEffect $ sourceP P.>-> p P.>-> P.mapM_ (\_ -> return ())

drainC :: C.Conduit Int Identity a -> ()
drainC c = runIdentity $ (sourceC C.$= c) C.$$ C.sinkNull

drainCIO :: C.Conduit Int IO a -> IO ()
drainCIO c = (sourceC C.$= c) C.$$ C.mapM_ (\_ -> return ())

drainSC :: C.Sink Int Identity b -> ()
drainSC c = runIdentity $ void $! sourceC C.$$ c

drainS :: (S.Stream (S.Of Int) Identity () -> S.Stream (S.Of Int) Identity ())
    -> ()
drainS s = runIdentity $ S.effects $ sourceS & s

drainSIO :: (S.Stream (S.Of Int) IO () -> S.Stream (S.Of Int) IO ()) -> IO ()
drainSIO s = sourceS & s & S.mapM_ (\_ -> return ())

sourceM :: Monad m => M.SourceT m Int
sourceM = M.enumerateFromTo 1 value

sourceC :: Monad m => C.Producer m Int
sourceC = C.enumFromTo 1 value

sourceP :: Monad m => P.Producer' Int m ()
sourceP = P.each [1..value]

sourceS :: Monad m => S.Stream (S.Of Int) m ()
sourceS = S.each [1..value]

main :: IO ()
main =
  defaultMain
  [ bgroup "ops"
    [
        bgroup "map"
          [ bench "machines" $ whnf drainM (M.mapping (+1))
          , bench "streaming" $ whnf drainS (S.map (+1))
          , bench "pipes" $ whnf drainP (P.map (+1))
          , bench "conduit" $ whnf drainC (C.map (+1))
          ]
      , bgroup "drop"
          [ bench "machines" $ whnf drainM (M.dropping value)
          , bench "streaming" $ whnf drainS (S.drop value)
          , bench "pipes" $ whnf drainP (P.drop value)
          , bench "conduit" $ whnf drainC (C.drop value)
          ]
      , bgroup "dropWhile"
          [ bench "machines" $ whnf drainM (M.droppingWhile (<= value))
          , bench "streaming" $ whnf drainS (S.dropWhile (<= value))
          , bench "pipes" $ whnf drainP (P.dropWhile (<= value))
          , bench "conduit" $ whnf drainC (CC.dropWhile (<= value))
          ]
      , bgroup "scan"
          [ bench "machines" $ whnf drainM (M.scan (+) 0)
          , bench "streaming" $ whnf drainS (S.scan (+) 0 id)
          , bench "pipes" $ whnf drainP (P.scan (+) 0 id)
          , bench "conduit" $ whnf drainC (CC.scanl (+) 0)
          ]
      , bgroup "take"
          [ bench "machines" $ whnf drainM (M.taking value)
          , bench "streaming" $ whnf drainS (S.take value)
          , bench "pipes" $ whnf drainP (P.take value)
          , bench "conduit" $ whnf drainC (C.isolate value)
          ]
      , bgroup "takeWhile"
          [ bench "machines" $ whnf drainM (M.takingWhile (<= value))
          , bench "streaming" $ whnf drainS (S.takeWhile (<= value))
          , bench "pipes" $ whnf drainP (P.takeWhile (<= value))
          , bench "conduit" $ whnf drainC (CC.takeWhile (<= value))
          ]
      , bgroup "fold"
          [ bench "machines" $ whnf drainM (M.fold (+) 0)
          , bench "streaming" $ whnf runIdentity $ (S.fold (+) 0 id) sourceS
          , bench "pipes" $ whnf runIdentity $ (P.fold (+) 0 id) sourceP
          , bench "conduit" $ whnf drainSC (C.fold (+) 0)
          ]
      , bgroup "filter"
          [ bench "machines" $ whnf drainM (M.filtered even)
          , bench "streaming" $ whnf drainS (S.filter even)
          , bench "pipes" $ whnf drainP (P.filter even)
          , bench "conduit" $ whnf drainC (C.filter even)
          ]
      , bgroup "mapM"
          [ bench "machines" $ whnf drainM (M.autoM Identity)
          , bench "streaming" $ whnf drainS (S.mapM Identity)
          , bench "pipes" $ whnf drainP (P.mapM Identity)
          , bench "conduit" $ whnf drainC (C.mapM Identity)
          ]
      , bgroup "zip"
          [ bench "machines" $ whnf (\x -> runIdentity $ M.runT_ x)
              (M.capT sourceM sourceM M.zipping)
          , bench "streaming" $ whnf (\x -> runIdentity $ S.effects $ x)
              (S.zip sourceS sourceS)
          , bench "pipes" $ whnf (\x -> runIdentity $ P.runEffect $ P.for x P.discard)
              (P.zip sourceP sourceP)
          , bench "conduit" $ whnf (\x -> runIdentity $ x C.$$ C.sinkNull)
              (C.getZipSource $ (,) <$> C.ZipSource sourceC <*> C.ZipSource sourceC)
          ]
      , bgroup "concat"
          [ bench "machines" $ whnf drainM (M.mapping (replicate 3) M.~> M.asParts)
          , bench "streaming" $ whnf drainS (S.concat . S.map (replicate 3))
          , bench "pipes" $ whnf drainP (P.map (replicate 3) P.>-> P.concat)
          , bench "conduit" $ whnf drainC (C.map (replicate 3) C.$= C.concat)
          ]
      , bgroup "last"
          [ bench "machines" $ whnf drainM (M.final)
          , bench "streaming" $ whnf runIdentity $ S.last sourceS
          , bench "pipes" $ whnf runIdentity $ P.last sourceP
          ]
  ]
  , bgroup "compose"
    [
      -- A fair comparison without fusion
      -- Assuming, fusion won't be able to combine effectful ops
      let f x =
              if (x `mod` 4 == 0)
              then
                  randomIO
              else return x

          m = M.autoM f
          s = S.mapM f
          p = P.mapM f
          c = C.mapM f
      in bgroup "mapM-randomIO"
        [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
        , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
        ]

      -- A fair comparison without fusion
      -- Assuming, fusion won't be able to combine effectful ops
    , let m = M.autoM return
          s = S.mapM return
          p = P.mapM return
          c = C.mapM return
      in bgroup "mapM"
        [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
        , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
        ]

    , let m = M.mapping (subtract 1) M.~> M.filtered (<= value)
          s = S.filter (<= value) . S.map (subtract 1)
          p = P.map (subtract 1)  P.>-> P.filter (<= value)
          c = C.map (subtract 1)  C.=$= C.filter (<= value)
      in bgroup "map-filter"
        [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
        , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
        ]

    -- Compose multiple ops, all stages letting everything through
    -- IO monad makes a big difference especially for machines
    , let m = M.filtered (<= value)
          s = S.filter (<= value)
          p = P.filter (<= value)
          c = C.filter (<= value)
      in bgroup "passing-filters"
        [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
        , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
        ]

      -- how filtering affects the subsequent composition
    , let m = M.filtered (> value)
          s = S.filter   (> value)
          p = P.filter   (> value)
          c = C.filter   (> value)
      in bgroup "blocking-filters"
        [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
        , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
        ]
    ]

  , bgroup "compose-Identity"
    [
      let m = M.autoM return
          s = S.mapM return
          p = P.mapM return
          c = C.mapM return
      in bgroup "mapM"
        [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnf drainS $ \x -> s x & s & s & s
        , bench "pipes"     $ whnf drainP $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnf drainC $ c C.=$= c C.=$= c C.=$= c
        ]

    , let m = M.mapping (subtract 1) M.~> M.filtered (<= value)
          s = S.filter (<= value) . S.map (subtract 1)
          p = P.map (subtract 1)  P.>-> P.filter (<= value)
          c = C.map (subtract 1)  C.=$= C.filter (<= value)
      in bgroup "map-filter"
        [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnf drainS $ \x -> s x & s & s & s
        , bench "pipes"     $ whnf drainP $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnf drainC $ c C.=$= c C.=$= c C.=$= c
        ]

    , let m = M.filtered (<= value)
          s = S.filter (<= value)
          p = P.filter (<= value)
          c = C.filter (<= value)
      in bgroup "passing-filters"
        [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnf drainS $ \x -> s x & s & s & s
        , bench "pipes"     $ whnf drainP $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnf drainC $ c C.=$= c C.=$= c C.=$= c
        ]

    , let m = M.filtered (> value)
          s = S.filter   (> value)
          p = P.filter   (> value)
          c = C.filter   (> value)
      in bgroup "blocking-filters"
        [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
        , bench "streaming" $ whnf drainS $ \x -> s x & s & s & s
        , bench "pipes"     $ whnf drainP $ p P.>-> p P.>-> p P.>-> p
        , bench "conduit"   $ whnf drainC $ c C.=$= c C.=$= c C.=$= c
        ]
    ]

  , bgroup "compose-study"
    [
    -- Scaling with same operation in sequence
      let f = M.filtered (<= value)
      in bgroup "machines-filters"
        [ bench "1" $ whnf drainM f
        , bench "2" $ whnf drainM $ f M.~> f
        , bench "3" $ whnf drainM $ f M.~> f M.~> f
        , bench "4" $ whnf drainM $ f M.~> f M.~> f M.~> f
        ]
    , let f = S.filter (<= value)
      in bgroup "streaming-filters"
        [ bench "1" $ whnf drainS (\x -> f x)
        , bench "2" $ whnf drainS $ \x -> f x & f
        , bench "3" $ whnf drainS $ \x -> f x & f & f
        , bench "4" $ whnf drainS $ \x -> f x & f & f & f
        ]
    , let f = P.filter (<= value)
      in bgroup "pipes-filters"
        [ bench "1" $ whnf drainP f
        , bench "2" $ whnf drainP $ f P.>-> f
        , bench "3" $ whnf drainP $ f P.>-> f P.>-> f
        , bench "4" $ whnf drainP $ f P.>-> f P.>-> f P.>-> f
        ]
    , let f = C.filter (<= value)
      in bgroup "conduit-filters"
        [ bench "1" $ whnf drainC f
        , bench "2" $ whnf drainC $ f C.=$= f
        , bench "3" $ whnf drainC $ f C.=$= f C.=$= f
        , bench "4" $ whnf drainC $ f C.=$= f C.=$= f C.=$= f
        ]

    , let f = M.mapping (subtract 1) M.~> M.filtered (<= value)
      in bgroup "machines-map-filter"
        [ bench "1" $ whnf drainM f
        , bench "2" $ whnf drainM $ f M.~> f
        , bench "3" $ whnf drainM $ f M.~> f M.~> f
        , bench "4" $ whnf drainM $ f M.~> f M.~> f M.~> f
        ]
    , let f = S.filter (<= value) . S.map (subtract 1)
      in bgroup "streaming-map-filter"
        [ bench "1" $ whnf drainS (\x -> f x)
        , bench "2" $ whnf drainS $ \x -> f x & f
        , bench "3" $ whnf drainS $ \x -> f x & f & f
        , bench "4" $ whnf drainS $ \x -> f x & f & f & f
        ]
    , let f = P.map (subtract 1)  P.>-> P.filter (<= value)
      in bgroup "pipes-map-filter"
        [ bench "1" $ whnf drainP f
        , bench "2" $ whnf drainP $ f P.>-> f
        , bench "3" $ whnf drainP $ f P.>-> f P.>-> f
        , bench "4" $ whnf drainP $ f P.>-> f P.>-> f P.>-> f
        ]
    , let f = C.map (subtract 1)  C.=$= C.filter (<= value)
      in bgroup "conduit-map-filter"
        [ bench "1" $ whnf drainC f
        , bench "2" $ whnf drainC $ f C.=$= f
        , bench "3" $ whnf drainC $ f C.=$= f C.=$= f
        , bench "4" $ whnf drainC $ f C.=$= f C.=$= f C.=$= f
        ]

    , let f = M.filtered (> value)
      in bgroup "machines-blocking-filter"
        [ bench "1" $ whnf drainM f
        , bench "2" $ whnf drainM $ f M.~> f
        , bench "3" $ whnf drainM $ f M.~> f M.~> f
        , bench "4" $ whnf drainM $ f M.~> f M.~> f M.~> f
        ]
    , let f = S.filter (> value)
      in bgroup "streaming-blocking-filter"
        [ bench "1" $ whnf drainS (\x -> f x)
        , bench "2" $ whnf drainS $ \x -> f x & f
        , bench "3" $ whnf drainS $ \x -> f x & f & f
        , bench "4" $ whnf drainS $ \x -> f x & f & f & f
        ]
    , let f = P.filter (> value)
      in bgroup "pipes-blocking-filter"
        [ bench "1" $ whnf drainP f
        , bench "2" $ whnf drainP $ f P.>-> f
        , bench "3" $ whnf drainP $ f P.>-> f P.>-> f
        , bench "4" $ whnf drainP $ f P.>-> f P.>-> f P.>-> f
        ]
    , let f = C.filter (> value)
      in bgroup "conduit-blocking-filter"
        [ bench "1" $ whnf drainC f
        , bench "2" $ whnf drainC $ f C.=$= f
        , bench "3" $ whnf drainC $ f C.=$= f C.=$= f
        , bench "4" $ whnf drainC $ f C.=$= f C.=$= f C.=$= f
        ]
    ]
  , bgroup "toList"
    [
      bgroup "Identity"
          [ bench "machines"  $ whnf (length . runIdentity) $ M.runT sourceM
          , bench "streaming" $ whnf (length . runIdentity)
                              $ S.toList sourceS >>= (\(xs S.:> _) -> return xs)
          , bench "pipes"     $ whnf (length . runIdentity) $ P.toListM sourceP
          , bench "conduit"   $ whnf (length . runIdentity)
                              $ sourceC C.$$ CC.sinkList
          ]
      , bgroup "IO"
          [ bench "machines"  $ whnfIO $ M.runT sourceM
          , bench "streaming" $ whnfIO $ S.toList sourceS
          , bench "pipes"     $ whnfIO $ P.toListM sourceP
          , bench "conduit"   $ whnfIO $ sourceC C.$$ CC.sinkList
          ]
    ]
  ]
