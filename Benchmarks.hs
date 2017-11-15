{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes          #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Gauge
import Data.Foldable (msum)
import Data.Function ((&))
--import System.Random (randomIO)
import System.Random (randomRIO)

import qualified Asyncly           as A
import qualified Asyncly.Prelude   as A
import qualified Data.Conduit      as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as C
import qualified List.Transformer  as L
import qualified ListT             as LT
import qualified Control.Monad.Logic as LG
import qualified Data.Machine      as M
import qualified Pipes             as P
import qualified Pipes.Prelude     as P
import qualified Streaming.Prelude as S
import qualified Conduit.Simple    as SC

getRandom :: MonadIO m => m Int
getRandom =  liftIO $ randomRIO (1,1000)

value, maxValue :: Int
value = 1000000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Asyncly
-------------------------------------------------------------------------------

sourceA :: MonadIO m => A.StreamT m Int
sourceA = getRandom >>= \v -> A.each [v..v+value]

-- Category composition
runIOA :: A.StreamT IO Int -> (A.StreamT IO Int -> A.StreamT IO Int) -> IO ()
runIOA s t = A.runStreamT $ s & t

{-
-- Monadic composition
runIOA_M :: A.StreamT IO Int -> (Int -> A.StreamT IO Int) -> IO ()
runIOA_M s t = A.runStreamT $ s >>= t
-}

-------------------------------------------------------------------------------
-- streaming
-------------------------------------------------------------------------------

sourceS :: MonadIO m => S.Stream (S.Of Int) m ()
sourceS = getRandom >>= \v -> S.each [v..v+value]

runIOS :: S.Stream (S.Of Int) IO ()
    -> (S.Stream (S.Of Int) IO () -> S.Stream (S.Of Int) IO ()) -> IO ()
runIOS s t = s & t & S.mapM_ (\_ -> return ())

-------------------------------------------------------------------------------
-- simple-conduit
-------------------------------------------------------------------------------

sourceSC :: MonadIO m => SC.Source m Int
sourceSC = getRandom >>= \v -> SC.enumFromToC v (v + value)

runIOSC :: SC.Source IO Int -> SC.Conduit Int IO a -> IO ()
runIOSC s t = s SC.$= t SC.$$ SC.mapM_C (\_ -> return ())

-------------------------------------------------------------------------------
-- conduit
-------------------------------------------------------------------------------

sourceC :: MonadIO m => C.Source m Int
sourceC = getRandom >>= \v -> C.enumFromTo v (v + value)

runIOC :: C.Source IO Int -> C.Conduit Int IO a -> IO ()
runIOC s t = s C.$= t C.$$ C.mapM_ (\_ -> return ())

-------------------------------------------------------------------------------
-- pipes
-------------------------------------------------------------------------------

sourceP :: MonadIO m => P.Producer' Int m ()
sourceP = getRandom >>= \v -> P.each [v..v+value]

runIOP :: P.Producer' Int IO () -> P.Proxy () Int () a IO () -> IO ()
runIOP s t = P.runEffect $ s P.>-> t P.>-> P.mapM_ (\_ -> return ())

-------------------------------------------------------------------------------
-- machines
-------------------------------------------------------------------------------

sourceM :: Monad m => Int -> M.SourceT m Int
sourceM v = M.enumerateFromTo v (v + value)

runIOM :: M.SourceT IO Int -> M.ProcessT IO Int o -> IO ()
runIOM s t = M.runT_ (s M.~> t)

-------------------------------------------------------------------------------
-- list-transformer
-------------------------------------------------------------------------------

sourceL :: MonadIO m => L.ListT m Int
sourceL = getRandom >>= \v -> L.select [v..v+value]

runIOL :: L.ListT IO Int -> (Int -> L.ListT IO Int) -> IO ()
runIOL s t = L.runListT (s >>= t)

-------------------------------------------------------------------------------
-- list-t
-------------------------------------------------------------------------------

sourceLT :: MonadIO m => LT.ListT m Int
sourceLT = getRandom >>= \v -> LT.fromFoldable [v..v+value]

runIOLT :: LT.ListT IO Int -> (Int -> LT.ListT IO Int) -> IO ()
runIOLT s t = LT.traverse_ (\_ -> return ()) (s >>= t)

-------------------------------------------------------------------------------
-- logict
-------------------------------------------------------------------------------

sourceLG :: Monad m => Int -> LG.LogicT m Int
sourceLG v = msum $ map return [v..v+value]

runIOLG :: LG.LogicT IO Int -> (Int -> LG.LogicT IO Int) -> IO ()
runIOLG s t = LG.observeAllT (s >>= t) >> return ()

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
  [ bgroup "elimination"
    [
      bgroup "null"
        [
          bench "conduit"          $ nfIO $ sourceC C.$$ C.mapM_ (\_ -> return ())
        , bench "pipes"            $ nfIO $ P.runEffect $ sourceP P.>-> P.mapM_ (\_ -> return ())
        , bench "machines"         $ nfIO $ getRandom >>= \v -> M.runT_ (sourceM v)
        , bench "streaming"        $ nfIO $ runIOS sourceS id
        , bench "asyncly"          $ nfIO $ runIOA sourceA id
        , bench "simple-conduit"   $ nfIO $ sourceSC SC.$$ SC.mapM_C (\_ -> return ())
        , bench "logict"           $ nfIO $ getRandom >>= \v -> LG.observeAllT (sourceLG v) >> return ()
        , bench "list-t"           $ nfIO $ LT.traverse_ (\_ -> return ()) sourceLT
        , bench "list-transformer" $ nfIO $ L.runListT sourceL
        ]
    , bgroup "toList"
          [
            bench "conduit"   $ nfIO $ sourceC C.$$ CC.sinkList
          , bench "pipes"     $ nfIO $ P.toListM sourceP
          , bench "machines"  $ nfIO $ getRandom >>= \v -> M.runT (sourceM v)
          , bench "streaming" $ whnfIO $ S.toList sourceS
          , bench "asyncly"   $ nfIO $ A.toList sourceA
          , bench "simple-conduit" $ nfIO $ sourceSC SC.$$ SC.sinkList
          , bench "logict"         $ nfIO $ getRandom >>= \v -> LG.observeAllT (sourceLG v) >> return ()
          , bench "list-t"         $ nfIO $ LT.toList sourceLT
          -- , bench "list-transformer" $ nfIO $ toList sourceL
          ]
    , bgroup "fold"
        [ bench "conduit"   $ nfIO   $ sourceC C.$$ (C.fold (+) 0)
        , bench "pipes"     $ nfIO   $ P.fold (+) 0 id sourceP
        , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.fold (+) 0)
        , bench "streaming" $ whnfIO $ S.fold (+) 0 id sourceS
        , bench "asyncly"   $ nfIO   $ A.foldl (+) 0 id sourceA
        , bench "list-transformer" $ nfIO $ L.fold (+) 0 id sourceL
        ]
    , bgroup "scan"
        [ bench "conduit" $ nfIO $ runIOC sourceC (CC.scanl (+) 0)
        , bench "pipes" $ nfIO $ runIOP sourceP (P.scan (+) 0 id)
        , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.scan (+) 0)
        , bench "streaming" $ whnfIO $ runIOS sourceS (S.scan (+) 0 id)
        ]
    , bgroup "last"
          [ bench "pipes" $ nfIO $ P.last sourceP
          , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.final)
          , bench "streaming" $ whnfIO $ S.last sourceS
          , bench "asyncly" $ nfIO $ A.last sourceA
          ]
    , bgroup "concat"
          [ bench "conduit" $ nfIO $ runIOC sourceC (C.map (replicate 3) C.$= C.concat)
          , bench "pipes" $ nfIO $ runIOP sourceP (P.map (replicate 3) P.>-> P.concat)
          , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.mapping (replicate 3) M.~> M.asParts)
          , bench "streaming" $ nfIO $ runIOS sourceS (S.concat . S.map (replicate 3))
          ]
    ]
    , bgroup "transformation"
        [ bgroup "map"
          [ bench "conduit" $ nfIO $ runIOC sourceC (C.map (+1))
          , bench "pipes" $ nfIO $ runIOP sourceP (P.map (+1))
          , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.mapping (+1))
          , bench "streaming" $ whnfIO $ runIOS sourceS (S.map (+1))
          , bench "asyncly" $ nfIO $ runIOA sourceA (fmap (+1))
          , bench "simple-conduit" $ nfIO $ runIOSC sourceSC (SC.mapC (+1))
          , bench "list-transformer" $ nfIO $ runIOL sourceL (lift . return . (+1))
          ]
        , bgroup "mapM"
          [ bench "conduit" $ nfIO $ runIOC sourceC (C.mapM return)
          , bench "pipes" $ nfIO $ runIOP sourceP (P.mapM return)
          , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.autoM return)
          , bench "streaming" $ whnfIO $ runIOS sourceS (S.mapM return)
          , bench "asyncly" $ nfIO $ runIOA sourceA (A.mapM return)
          ]
        ]
    , bgroup "filtering"
        [ bgroup "filter"
          [ bench "conduit" $ nfIO $ runIOC sourceC (C.filter even)
          , bench "pipes" $ nfIO $ runIOP sourceP (P.filter even)
          , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.filtered even)
          , bench "streaming" $ whnfIO $ runIOS sourceS (S.filter even)
          , bench "asyncly" $ nfIO $ runIOA sourceA (A.filter even)
          ]
          -- XXX variance need to be fixed, value used is not correct
        , bgroup "take"
          [ bench "conduit" $ nfIO $ runIOC sourceC (C.isolate value)
          , bench "pipes" $ nfIO $ runIOP sourceP (P.take value)
          , bench "machines" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.taking value)
          , bench "streaming" $ whnfIO $ runIOS sourceS (S.take value)
          , bench "asyncly" $ nfIO $ runIOA sourceA (A.take value)
          -- , bench "list-transformer" $ nfIO $ (runIdentity . L.runListT) (L.take value sourceL :: L.ListT Identity Int)
          ]
        , bgroup "takeWhile"
          [ bench "conduit"   $ nfIO $ runIOC sourceC (CC.takeWhile (<= value))
          , bench "pipes"     $ nfIO $ runIOP sourceP (P.takeWhile (<= value))
          , bench "machines"  $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.takingWhile (<= value))
          , bench "streaming" $ whnfIO $ runIOS sourceS (S.takeWhile (<= value))
          , bench "asyncly"   $ nfIO $ runIOA sourceA (A.takeWhile (<= value))
          ]
        , bgroup "drop"
          [ bench "conduit"   $ nfIO $ runIOC sourceC (C.drop value)
          , bench "pipes"     $ nfIO $ runIOP sourceP (P.drop value)
          , bench "machines"  $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.dropping value)
          , bench "streaming" $ whnfIO $ runIOS sourceS (S.drop value)
          , bench "asyncly"   $ nfIO $ runIOA sourceA (A.drop value)
          -- , bench "simple-conduit" $ whnf drainSC (SC.dropC value)
          --, bench "list-transformer" $ whnf (runIdentity . L.runListT) (L.drop value sourceL :: L.ListT Identity Int)
          ]
        , bgroup "dropWhile"
          [ bench "conduit"   $ nfIO $ runIOC sourceC (CC.dropWhile (<= value))
          , bench "pipes"     $ nfIO $ runIOP sourceP (P.dropWhile (<= value))
          , bench "machines"  $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) (M.droppingWhile (<= value))
          , bench "streaming" $ whnfIO $ runIOS sourceS (S.dropWhile (<= value))
          , bench "asyncly"   $ nfIO $ runIOA sourceA (A.dropWhile (<= value))
          ]
        ]
    , bgroup "zip"
        [ bench "conduit" $ nfIO $ (C.getZipSource $ (,) <$> C.ZipSource sourceC <*> C.ZipSource sourceC) C.$$ C.sinkNull
        , bench "pipes" $ nfIO $ P.runEffect $ P.for (P.zip sourceP sourceP) P.discard
        , bench "machines" $ nfIO $ getRandom >>= \v1 -> getRandom >>= \v2 -> M.runT_ (M.capT (sourceM v1) (sourceM v2) M.zipping)
        , bench "streaming" $ whnfIO $ S.effects (S.zip sourceS sourceS)
        , bench "asyncly" $ nfIO $ A.runStreamT $ (A.zipWith (,) sourceA sourceA)
        ]
    -- Composing multiple stages of a pipeline
    , bgroup "compose"
        [
        {-
          let f x =
                  if (x `mod` 4 == 0)
                  then
                      randomIO
                  else return x
        -}
          let f = return
              c = C.mapM f
              p = P.mapM f
              m = M.autoM f
              s = S.mapM f
              a = A.mapM f
              lb = lift . f
              l = lift . f
              lg = lift . f
          in bgroup "mapM"
            [ bench "conduit"   $ nfIO $ runIOC sourceC $ c C.=$= c C.=$= c C.=$= c
            , bench "pipes"     $ nfIO $ runIOP sourceP $ p P.>-> p P.>-> p P.>-> p
            , bench "machines"  $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) $ m M.~> m M.~> m M.~> m
            , bench "streaming" $ whnfIO $ runIOS sourceS $ \x -> s x & s & s & s
            , bench "asyncly"   $ nfIO $ runIOA sourceA $ \x -> a x & a & a & a
            , bench "list-t"    $ nfIO $ runIOLT sourceLT $ \x -> lb x >>= lb >>= lb >>= lb
            , bench "list-transformer" $ nfIO $ runIOL sourceL $ \x -> l x >>= l >>= l >>= l
            , bench "logict"    $ nfIO $ getRandom >>= \v -> runIOLG (sourceLG v) $ \x -> lg x >>= lg >>= lg >>= lg
            ]

        , let m = M.mapping (subtract 1) M.~> M.filtered (<= value)
              s = S.filter (<= value) . S.map (subtract 1)
              a = A.filter (<= value) . fmap (subtract 1)
              p = P.map (subtract 1)  P.>-> P.filter (<= value)
              c = C.map (subtract 1)  C.=$= C.filter (<= value)
          in bgroup "map-filter"
            [ bench "conduit"   $ nfIO $ runIOC sourceC $ c C.=$= c C.=$= c C.=$= c
            , bench "pipes"     $ nfIO $ runIOP sourceP $ p P.>-> p P.>-> p P.>-> p
            , bench "machines"  $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) $ m M.~> m M.~> m M.~> m
            , bench "streaming" $ whnfIO $ runIOS sourceS $ \x -> s x & s & s & s
            , bench "asyncly" $ nfIO $ runIOA sourceA $ \x -> a x & a & a & a
            ]

        -- Compose multiple ops, all stages letting everything through
        -- IO monad makes a big difference especially for machines
        , let m = M.filtered (<= value)
              a = A.filter (<= value)
              s = S.filter (<= value)
              p = P.filter (<= value)
              c = C.filter (<= value)
          in bgroup "passing-filters"
            [ bench "conduit"   $ nfIO $ runIOC sourceC $ c C.=$= c C.=$= c C.=$= c
            , bench "pipes"     $ nfIO $ runIOP sourceP $ p P.>-> p P.>-> p P.>-> p
            , bench "machines"  $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) $ m M.~> m M.~> m M.~> m
            , bench "streaming" $ whnfIO $ runIOS sourceS $ \x -> s x & s & s & s
            , bench "asyncly" $ nfIO $ runIOA sourceA $ \x -> a x & a & a & a
            ]

          -- how filtering affects the subsequent composition
        , let m = M.filtered (> value)
              a = A.filter   (> value)
              s = S.filter   (> value)
              p = P.filter   (> value)
              c = C.filter   (> value)
          in bgroup "blocking-filters"
            [ bench "conduit"   $ nfIO $ runIOC sourceC $ c C.=$= c C.=$= c C.=$= c
            , bench "pipes"     $ nfIO $ runIOP sourceP $ p P.>-> p P.>-> p P.>-> p
            , bench "machines"  $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) $ m M.~> m M.~> m M.~> m
            , bench "streaming" $ whnfIO $ runIOS sourceS $ \x -> s x & s & s & s
            , bench "asyncly" $ nfIO $ runIOA sourceA $ \x -> a x & a & a & a
            ]
        ]
    , bgroup "compose-scaling"
        [
        -- Scaling with same operation in sequence
          let f = M.filtered (<= maxValue)
          in bgroup "machines-filters"
            [ bench "1" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) f
            , bench "2" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) $ f M.~> f
            , bench "3" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) $ f M.~> f M.~> f
            , bench "4" $ nfIO $ getRandom >>= \v -> runIOM (sourceM v) $ f M.~> f M.~> f M.~> f
            ]
        , let f = A.filter (<= maxValue)
          in bgroup "asyncly-filters"
            [ bench "1" $ nfIO $ runIOA sourceA (\x -> f x)
            , bench "2" $ nfIO $ runIOA sourceA $ \x -> f x & f
            , bench "3" $ nfIO $ runIOA sourceA $ \x -> f x & f & f
            , bench "4" $ nfIO $ runIOA sourceA $ \x -> f x & f & f & f
            ]
        , let f = S.filter (<= maxValue)
          in bgroup "streaming-filters"
            [ bench "1" $ whnfIO $ runIOS sourceS (\x -> f x)
            , bench "2" $ whnfIO $ runIOS sourceS $ \x -> f x & f
            , bench "3" $ whnfIO $ runIOS sourceS $ \x -> f x & f & f
            , bench "4" $ whnfIO $ runIOS sourceS $ \x -> f x & f & f & f
            ]
        , let f = P.filter (<= maxValue)
          in bgroup "pipes-filters"
            [ bench "1" $ nfIO $ runIOP sourceP f
            , bench "2" $ nfIO $ runIOP sourceP $ f P.>-> f
            , bench "3" $ nfIO $ runIOP sourceP $ f P.>-> f P.>-> f
            , bench "4" $ nfIO $ runIOP sourceP $ f P.>-> f P.>-> f P.>-> f
            ]
        , let f = C.filter (<= maxValue)
          in bgroup "conduit-filters"
            [ bench "1" $ nfIO $ runIOC sourceC f
            , bench "2" $ nfIO $ runIOC sourceC $ f C.=$= f
            , bench "3" $ nfIO $ runIOC sourceC $ f C.=$= f C.=$= f
            , bench "4" $ nfIO $ runIOC sourceC $ f C.=$= f C.=$= f C.=$= f
            ]
        ]
  ]
