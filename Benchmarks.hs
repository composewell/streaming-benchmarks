{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes          #-}
module Main (main) where

import Control.Monad (void)
import Control.Monad.Identity
import Control.Monad.Primitive (PrimState)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
--import Control.Monad.Trans.Control (MonadBaseControl)
import Gauge
import Gauge.Main
import Data.Foldable (msum)
import Data.Function ((&))
import Data.Word (Word32)
import System.Random (randomIO)
import qualified System.Random.MWC as MWC
import qualified System.Random as RND
import qualified Data.Vector.Unboxed as V

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

-- TBD try a monadic random stream rather than a purely generated one

value :: Int
value = 1000000

-------------------------------------------------------------------------------
-- Asyncly
-------------------------------------------------------------------------------

randomA :: Monad m => Int -> A.StreamT m Int
randomA n = do
    let g = RND.mkStdGen 0
    A.unfoldr' go (g, n)

    where
          go (g, 0) = Nothing
          go (g, n) =
            let (i, g') = RND.random g
             in Just $ (i, (g',  (n - 1)))


sourceA :: Monad m => Int -> A.StreamT m Int
--sourceA v = A.each [1..v]
sourceA = randomA
--sourceA = A.foldWith (A.<>) $ map return [1..value]

-- An evaluated source
sourceA_ :: Monad m => A.StreamT m Int
sourceA_ = sourceA value

-- Category composition
runA :: A.StreamT Identity Int -> (A.StreamT Identity Int -> A.StreamT Identity Int) -> ()
runA s t = runIdentity $ A.runStreamT $ s & t

runIOA :: A.StreamT IO Int -> (A.StreamT IO Int -> A.StreamT IO Int) -> IO ()
runIOA s t = A.runStreamT $ s & t

-- Monadic composition
runA_M :: A.StreamT Identity Int -> (Int -> A.StreamT Identity Int) -> ()
runA_M s t = runIdentity $ A.runStreamT $ s >>= t

runIOA_M :: A.StreamT IO Int -> (Int -> A.StreamT IO Int) -> IO ()
runIOA_M s t = A.runStreamT $ s >>= t

-------------------------------------------------------------------------------
-- streaming
-------------------------------------------------------------------------------

randomS :: Monad m => Int -> S.Stream (S.Of Int) m ()
randomS n = do
    let g = RND.mkStdGen 0
    go g n

    where
          go g 0 = return ()
          go g n = do
            let (i, g') = RND.random g
            S.yield i >> go g'  (n - 1)

sourceS :: Monad m => Int -> S.Stream (S.Of Int) m ()
--sourceS v = S.each [1..v]
sourceS = randomS

sourceS_ :: Monad m => S.Stream (S.Of Int) m ()
sourceS_ = sourceS value

runS :: S.Stream (S.Of Int) Identity ()
    -> (S.Stream (S.Of Int) Identity () -> S.Stream (S.Of Int) Identity ())
    -> ()
runS s t = runIdentity $ S.effects $ s & t

runIOS :: S.Stream (S.Of Int) IO ()
    -> (S.Stream (S.Of Int) IO () -> S.Stream (S.Of Int) IO ()) -> IO ()
runIOS s t = s & t & S.mapM_ (\_ -> return ())

-------------------------------------------------------------------------------
-- simple-conduit
-------------------------------------------------------------------------------

randomSC :: Monad m => Int -> SC.Source m Int
randomSC n = do
    let g = RND.mkStdGen 0
    SC.unfoldC go (g, n)

    where
          go (g, 0) = Nothing
          go (g, n) =
            let (i, g') = RND.random g
             in Just $ (i, (g',  (n - 1)))

sourceSC :: Monad m => Int -> SC.Source m Int
--sourceSC v = SC.enumFromToC 1 v
sourceSC = randomSC

sourceSC_ :: Monad m => SC.Source m Int
sourceSC_ = sourceSC value

runSC :: SC.Source Identity Int -> SC.Conduit Int Identity a -> ()
runSC s t = runIdentity $ s SC.$= t SC.$$ SC.sinkNull

runIOSC :: SC.Source IO Int -> SC.Conduit Int IO a -> IO ()
runIOSC s t = s SC.$= t SC.$$ SC.mapM_C (\_ -> return ())

-------------------------------------------------------------------------------
-- conduit
-------------------------------------------------------------------------------

{-
randomC :: Monad IO => Word32 -> Int -> C.Source IO Int
randomC s n = do
    g <- MWC.initialize (V.singleton s)
    go g n

    where
    --      go :: MWC.Gen (PrimState (C.ConduitM () Int IO)) -> Int -> C.Source IO Int
          go g 0 = return ()
          go g n = do
            i <- MWC.uniform g
            C.yield i >> go g  (n - 1)
            -}

randomC :: Monad m => Int -> C.Source m Int
randomC n = do
    let g = RND.mkStdGen 0
    go g n

    where
          go g 0 = return ()
          go g n = do
            let (i, g') = RND.random g
            C.yield i >> go g'  (n - 1)

sourceC :: Monad m => Int -> C.Source m Int
--sourceC v = C.enumFromTo 1 v
sourceC = randomC

sourceC_ :: Monad m => C.Source m Int
sourceC_ = sourceC value

runC :: C.Producer Identity Int -> C.Conduit Int Identity a -> ()
runC s t = runIdentity $ s C.$= t C.$$ CC.sinkNull

runIOC :: C.Producer IO Int -> C.Conduit Int IO a -> IO ()
runIOC s t = s C.$= t C.$$ C.mapM_ (\_ -> return ())

-------------------------------------------------------------------------------
-- pipes
-------------------------------------------------------------------------------

randomP :: Monad m => Int -> P.Producer' Int m ()
randomP n = do
    let g = RND.mkStdGen 0
    go g n

    where
          go g 0 = return ()
          go g n = do
            let (i, g') = RND.random g
            P.yield i >> go g'  (n - 1)

sourceP :: Monad m => Int -> P.Producer' Int m ()
--sourceP v = P.each [1..v]
sourceP = randomP

sourceP_ :: Monad m => P.Producer' Int m ()
sourceP_ = sourceP value

runP :: P.Producer' Int Identity () -> P.Proxy () Int () a Identity () -> ()
runP s t = runIdentity $ P.runEffect $ P.for (s P.>-> t) P.discard

runIOP :: P.Producer' Int IO () -> P.Proxy () Int () a IO () -> IO ()
runIOP s t = P.runEffect $ s P.>-> t P.>-> P.mapM_ (\_ -> return ())

-------------------------------------------------------------------------------
-- machines
-------------------------------------------------------------------------------

randomM :: Monad m => Int -> M.SourceT m Int
randomM n = do
    let g = RND.mkStdGen 0
    M.unfoldT go (g, n)

    where
          go (g, 0) = return Nothing
          go (g, n) = do
            let (i, g') = RND.random g
            return $ Just $ (i, (g',  (n - 1)))

sourceM :: Monad m => Int -> M.SourceT m Int
--sourceM v = M.enumerateFromTo 1 v
sourceM = randomM

sourceM_ :: Monad m => M.SourceT m Int
sourceM_ = sourceM value

runM :: M.SourceT Identity Int -> M.ProcessT Identity Int o -> ()
runM s t = runIdentity $ M.runT_ (s M.~> t)

runIOM :: M.SourceT IO Int -> M.ProcessT IO Int o -> IO ()
runIOM s t = M.runT_ (s M.~> t)

-------------------------------------------------------------------------------
-- list-transformer
-------------------------------------------------------------------------------

randomL :: Monad m => Int -> L.ListT m Int
randomL n = do
    let g = RND.mkStdGen 0
    go (g, n)

    where
          go (g, 0) = L.ListT $ return L.Nil
          go (g, n) = L.ListT $
            let (i, g') = RND.random g
             in return $ L.Cons i (go (g', n - 1))

sourceL :: Monad m => Int -> L.ListT m Int
--sourceL v = L.select [1..v]
sourceL = randomL

sourceL_ :: Monad m => L.ListT m Int
sourceL_ = sourceL value

runL :: L.ListT Identity Int -> (Int -> L.ListT Identity Int) -> ()
runL s t = runIdentity $ L.runListT (s >>= t)

runIOL :: L.ListT IO Int -> (Int -> L.ListT IO Int) -> IO ()
runIOL s t = L.runListT (s >>= t)

-------------------------------------------------------------------------------
-- list-t
-------------------------------------------------------------------------------

randomLT :: Monad m => Int -> LT.ListT m Int
randomLT n = do
    let g = RND.mkStdGen 0
    LT.unfold go (g, n)

    where
          go (g, 0) = Nothing
          go (g, n) =
            let (i, g') = RND.random g
             in Just $ (i, (g',  (n - 1)))

sourceLT :: Monad m => Int -> LT.ListT m Int
--sourceLT v = LT.fromFoldable [1..v]
sourceLT = randomLT

sourceLT_ :: Monad m => LT.ListT m Int
sourceLT_ = sourceLT value

runLT :: LT.ListT Identity Int -> (Int -> LT.ListT Identity Int) -> ()
runLT s t = runIdentity $ LT.traverse_ (\_ -> return ()) (s >>= t)

runIOLT :: LT.ListT IO Int -> (Int -> LT.ListT IO Int) -> IO ()
runIOLT s t = LT.traverse_ (\_ -> return ()) (s >>= t)

-------------------------------------------------------------------------------
-- logict
-------------------------------------------------------------------------------

randomLG :: Monad m => Int -> LG.LogicT m Int
randomLG n = go (RND.mkStdGen 0) n
    where go g n = LG.LogicT $ \yld stp -> do
            if n == 0
            then stp
            else let (i, g') = RND.random g
                 in yld i ((LG.runLogicT (go g' (n - 1))) yld stp)

sourceLG :: Monad m => Int -> LG.LogicT m Int
--sourceLG v = msum $ map return [1..v]
sourceLG = randomLG

sourceLG_ :: Monad m => LG.LogicT m Int
sourceLG_ = sourceLG value

runLG :: LG.LogicT Identity Int -> (Int -> LG.LogicT Identity Int) -> ()
runLG s t = runIdentity $ LG.runLogicT (s >>= t) (\_ _ -> return ()) (return ())

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
      bgroup "Identity-null-pipe"
        [
        -- Monadic composition yields and awaits
        -- Implicit stream composition using special operators
          bench "conduit"          $ nf (\s -> runIdentity $ s C.$$ C.sinkNull) (sourceC value)
        , bench "pipes"            $ nf (\s -> runIdentity $ P.runEffect (P.for s P.discard)) (sourceP value)
        , bench "machines"         $ nf (\s -> runIdentity $ M.runT_ s) (sourceM value)

        -- Function style explicit stream transformation
        , bench "streaming"        $ nf (\s -> runS s id) (sourceS value)

        -- List transformer style monadic composition
        -- Function style explcit stream transformation
        , bench "asyncly"          $ nf (\s -> runA s id) (sourceA value)
        , bench "simple-conduit"   $ nf (\s -> runIdentity $ s SC.$$ SC.sinkNull) (sourceSC value)
        , bench "logict"           $ nf (\s -> runIdentity $ LG.runLogicT s (\_ _ -> return ()) (return ())) (sourceLG value)
        , bench "list-t"           $ nf (\s -> runIdentity $ LT.traverse_ (\_ -> return ()) s) (sourceLT value)
        , bench "list-transformer" $ nf (\s -> runIdentity $ L.runListT s) (sourceL value)
        ]
      -- Note: many libraries are significantly slower if we apply (sourceX
      -- value) instead of fusing the source API inside and applying value to
      -- the function being evaluated.
    , bgroup "Identity-null-pipe-fused"
        [
          bench "conduit"          $ nf (\v -> runIdentity $ (sourceC v) C.$$ C.sinkNull) value
        , bench "pipes"            $ nf (\v -> runIdentity $ P.runEffect (P.for (sourceP v) P.discard)) value
        , bench "machines"         $ nf (\v -> runIdentity $ M.runT_ (sourceM v)) value
        , bench "streaming"        $ nf (\v -> runS (sourceS v) id) value
        , bench "asyncly"          $ nf (\v -> runA (sourceA v) id) value
        , bench "simple-conduit"   $ nf (\v -> runIdentity $ (sourceSC v) SC.$$ SC.sinkNull) value
        , bench "logict"           $ nf (\v -> runIdentity $ LG.runLogicT (sourceLG v) (\_ _ -> return ()) (return ())) value
        , bench "list-t"           $ nf (\v -> runIdentity $ LT.traverse_ (\_ -> return ()) (sourceLT v)) value
        , bench "list-transformer" $ nf (\v -> runIdentity $ L.runListT (sourceL v)) value
        ]
    , bgroup "IO-null-pipe"
        [
          bench "conduit"          $ nfIO $ (sourceC value) C.$$ C.mapM_ (\_ -> return ())
          {-
        , bench "pipes"            $ nfIO (\s -> runIdentity $ P.runEffect (P.for s P.discard)) (sourceP value)
        , bench "machines"         $ nfIO (\s -> runIdentity $ M.runT_ s) (sourceM value)
        , bench "streaming"        $ nfIO (\s -> runS s id) (sourceS value)
        , bench "asyncly"          $ nfIO (\s -> runA s id) (sourceA value)
        , bench "simple-conduit"   $ nfIO (\s -> runIdentity $ s SC.$$ SC.sinkNull) (sourceSC value)
        , bench "logict"           $ nfIO (\s -> runIdentity $ LG.runLogicT s (\_ _ -> return ()) (return ())) (sourceLG value)
        , bench "list-t"           $ nfIO (\s -> runIdentity $ LT.traverse_ (\_ -> return ()) s) (sourceLT value)
        , bench "list-transformer" $ nfIO (\s -> runIdentity $ L.runListT s) (sourceL value)
        -}
        ]
        {-
    , bgroup "toList-Identity"
          [ bench "machines"  $ whnf (length . runIdentity) $ M.runT sourceM
          , bench "asyncly"   $ whnf (length . runIdentity) $ A.toList sourceA
          , bench "streaming" $ whnf (length . runIdentity)
                              $ S.toList sourceS >>= (\(xs S.:> _) -> return xs)
          , bench "pipes"     $ whnf (length . runIdentity) $ P.toListM sourceP
        --  , bench "conduit"   $ whnf (length . runIdentity)
        --                      $ sourceC C.$$ CC.sinkList
          , bench "simple-conduit" $ whnf (length . runIdentity)
                              $ sourceSC SC.$$ SC.sinkList
          ]
    , bgroup "toList-IO"
          [ bench "machines"  $ whnfIO $ M.runT sourceM
          , bench "asyncly"   $ whnfIO $ A.toList sourceA
          , bench "streaming" $ whnfIO $ S.toList sourceS
          , bench "pipes"     $ whnfIO $ P.toListM sourceP
        --  , bench "conduit"   $ whnfIO $ sourceC C.$$ CC.sinkList
          , bench "simple-conduit" $ whnfIO $ sourceSC SC.$$ SC.sinkList
          ]
    , bgroup "fold"
        [ bench "machines" $ whnf drainM (M.fold (+) 0)
        , bench "asyncly" $ whnf (\c -> runIdentity $! c sourceA) (A.foldl (+) 0 id)
        , bench "streaming" $ whnf (\c -> runIdentity $! c sourceS) (S.fold (+) 0 id)
        , bench "pipes" $ whnf (\c -> runIdentity $! c sourceP) (P.fold (+) 0 id)
        , bench "conduit" $ whnf (\c -> runIdentity $ void $! sourceC C.$$ c) (C.fold (+) 0)
        , bench "list-transformer" $ whnf (\c -> runIdentity $! c sourceL) (L.fold (+) 0 id) -- sourceL :: Identity Int)
        ]
    , bgroup "scan"
        [ bench "machines" $ whnf drainM (M.scan (+) 0)
        , bench "streaming" $ whnf drainS (S.scan (+) 0 id)
        , bench "pipes" $ whnf drainP (P.scan (+) 0 id)
        , bench "conduit" $ whnf drainC (CC.scanl (+) 0)
        ]
    , bgroup "last"
          [ bench "machines" $ whnf drainM (M.final)
          , bench "asyncly" $ whnf runIdentity $ A.last sourceA
          -- , bench "streaming" $ whnf runIdentity $ S.last sourceS
          --, bench "pipes" $ whnf runIdentity $ P.last sourceP
          ]
    , bgroup "concat"
          [ bench "machines" $ whnf drainM (M.mapping (replicate 3) M.~> M.asParts)
          , bench "streaming" $ whnf drainS (S.concat . S.map (replicate 3))
          , bench "pipes" $ whnf drainP (P.map (replicate 3) P.>-> P.concat)
          , bench "conduit" $ whnf drainC (C.map (replicate 3) C.$= C.concat)
          ]
          -}
    ]
    {-
    , bgroup "transformation"
        [ bgroup "map"
          [ bench "machines" $ nf drainM (M.mapping (+1))
          , bench "asyncly" $ nf drainA (fmap (+1))
          , bench "streaming" $ nf drainS (S.map (+1))
          , bench "pipes" $ nf drainP (P.map (+1))
          , bench "conduit" $ nf drainC (C.map (+1))
          -- , bench "simple-conduit" $ whnf drainSC (SC.mapC (+1))
          , bench "list-transformer" $ nf drainL (lift . return . (+1))
          ]
        , bgroup "mapM"
          [ bench "machines" $ whnf drainM (M.autoM Identity)
          , bench "asyncly" $ whnf drainA (A.mapM Identity)
          , bench "streaming" $ whnf drainS (S.mapM Identity)
          , bench "pipes" $ whnf drainP (P.mapM Identity)
          , bench "conduit" $ whnf drainC (C.mapM Identity)
          ]
        ]
    , bgroup "filtering"
        [ bgroup "filter"
          [ bench "machines" $ whnf drainM (M.filtered even)
          , bench "asyncly" $ whnf drainA (A.filter even)
          , bench "streaming" $ whnf drainS (S.filter even)
          , bench "pipes" $ whnf drainP (P.filter even)
          , bench "conduit" $ whnf drainC (C.filter even)
          ]
        , bgroup "take"
          [ bench "machines" $ whnf drainM (M.taking value)
          , bench "asyncly" $ whnf drainA (A.take value)
          , bench "streaming" $ whnf drainS (S.take value)
          , bench "pipes" $ whnf drainP (P.take value)
          , bench "conduit" $ whnf drainC (C.isolate value)
          , bench "list-transformer" $ whnf (runIdentity . L.runListT) (L.take value sourceL :: L.ListT Identity Int)
          ]
        , bgroup "takeWhile"
          [ bench "machines" $ whnf drainM (M.takingWhile (<= value))
          , bench "asyncly" $ whnf drainA (A.takeWhile (<= value))
          , bench "streaming" $ whnf drainS (S.takeWhile (<= value))
          , bench "pipes" $ whnf drainP (P.takeWhile (<= value))
          , bench "conduit" $ whnf drainC (CC.takeWhile (<= value))
          ]
        , bgroup "drop"
          [ bench "machines" $ whnf drainM (M.dropping value)
          , bench "asyncly" $ whnf drainA (A.drop value)
          , bench "streaming" $ whnf drainS (S.drop value)
          , bench "pipes" $ whnf drainP (P.drop value)
          , bench "conduit" $ whnf drainC (C.drop value)
          -- , bench "simple-conduit" $ whnf drainSC (SC.dropC value)
          --, bench "list-transformer" $ whnf (runIdentity . L.runListT) (L.drop value sourceL :: L.ListT Identity Int)
          ]
        , bgroup "dropWhile"
          [ bench "machines" $ whnf drainM (M.droppingWhile (<= value))
          , bench "asyncly" $ whnf drainA (A.dropWhile (<= value))
          , bench "streaming" $ whnf drainS (S.dropWhile (<= value))
          , bench "pipes" $ whnf drainP (P.dropWhile (<= value))
          , bench "conduit" $ whnf drainC (CC.dropWhile (<= value))
          ]
        ]
    , bgroup "zip"
        [ bench "machines" $ whnf (\x -> runIdentity $ M.runT_ x)
            (M.capT sourceM sourceM M.zipping)
        , bench "asyncly" $ whnf (\x -> runIdentity $ A.runStreamT $ x)
            (A.zipWith (,) sourceA sourceA)
        , bench "streaming" $ whnf (\x -> runIdentity $ S.effects $ x)
            (S.zip sourceS sourceS)
        , bench "pipes" $ whnf (\x -> runIdentity $ P.runEffect $ P.for x P.discard)
            (P.zip sourceP sourceP)
        , bench "conduit" $ whnf (\x -> runIdentity $ x C.$$ C.sinkNull)
            (C.getZipSource $ (,) <$> C.ZipSource sourceC <*> C.ZipSource sourceC)
        ]
    -- Composing multiple stages of a pipeline
    , bgroup "compose-IO"
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
              l = lift . f
              lb = lift . f
              lg = lift . f
              a = liftIO . f
          in bgroup "mapM-randomIO"
            [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
            , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
            , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c

            , bench "list-transformer"   $ whnfIO $ drainLIO $ \x -> l x >>= l >>= l >>= l
            , bench "list-t"    $ whnfIO $ drainLBIO $ \x -> lb x >>= lb >>= lb >>= lb
            , bench "logict"    $ whnfIO $ drainLGIO $ \x -> lg x >>= lg >>= lg >>= lg
            , bench "asyncly"   $ whnfIO $ drainAIO $ \x -> a x >>= a >>= a >>= a
            ]

          -- A fair comparison without fusion
          -- Assuming, fusion won't be able to combine effectful ops
        , let m = M.autoM return
              s = S.mapM return
              p = P.mapM return
              c = C.mapM return
              l = lift . return
              lb = lift . return
              lg = lift . return
              a = liftIO . return
          in bgroup "mapM"
            [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
            , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
            , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
            , bench "list-transformer"   $ whnfIO $ drainLIO $ \x -> l x >>= l >>= l >>= l
            , bench "list-t"    $ whnfIO $ drainLBIO $ \x -> lb x >>= lb >>= lb >>= lb
            , bench "logict"    $ whnfIO $ drainLGIO $ \x -> lg x >>= lg >>= lg >>= lg
            , bench "asyncly"   $ whnfIO $ drainAIO $ \x -> a x >>= a >>= a >>= a
            ]

        , let m = M.mapping (subtract 1) M.~> M.filtered (<= value)
              s = S.filter (<= value) . S.map (subtract 1)
              a = A.filter (<= value) . fmap (subtract 1)
              p = P.map (subtract 1)  P.>-> P.filter (<= value)
              c = C.map (subtract 1)  C.=$= C.filter (<= value)
          in bgroup "map-filter"
            [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
            , bench "asyncly" $ whnfIO $ drainAIOStream $ \x -> a x & a & a & a
            , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
            , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
            ]

        -- Compose multiple ops, all stages letting everything through
        -- IO monad makes a big difference especially for machines
        , let m = M.filtered (<= value)
              a = A.filter (<= value)
              s = S.filter (<= value)
              p = P.filter (<= value)
              c = C.filter (<= value)
          in bgroup "passing-filters"
            [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
            , bench "asyncly"   $ whnfIO $ drainAIOStream $ \x -> a x & a & a & a
            , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
            , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
            ]

          -- how filtering affects the subsequent composition
        , let m = M.filtered (> value)
              a = A.filter   (> value)
              s = S.filter   (> value)
              p = P.filter   (> value)
              c = C.filter   (> value)
          in bgroup "blocking-filters"
            [ bench "machines"  $ whnfIO $ drainMIO $ m M.~> m M.~> m M.~> m
            , bench "asyncly"   $ whnfIO $ drainAIOStream $ \x -> a x & a & a & a
            , bench "streaming" $ whnfIO $ drainSIO $ \x -> s x & s & s & s
            , bench "pipes"     $ whnfIO $ drainPIO $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnfIO $ drainCIO $ c C.=$= c C.=$= c C.=$= c
            ]
        ]
    , bgroup "compose-Identity"
        [
          let m = M.autoM return
              a = A.mapM return
              s = S.mapM return
              p = P.mapM return
              c = C.mapM return
          in bgroup "mapM"
            [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
            , bench "asyncly"   $ whnf drainA $ \x -> a x & a & a & a
            , bench "streaming" $ whnf drainS $ \x -> s x & s & s & s
            , bench "pipes"     $ whnf drainP $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnf drainC $ c C.=$= c C.=$= c C.=$= c
            ]

        , let m = M.mapping (subtract 1) M.~> M.filtered (<= value)
              a = A.filter (<= value) . fmap (subtract 1)
              s = S.filter (<= value) . S.map (subtract 1)
              p = P.map (subtract 1)  P.>-> P.filter (<= value)
              c = C.map (subtract 1)  C.=$= C.filter (<= value)
          in bgroup "map-filter"
            [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
            , bench "asyncly"   $ whnf drainA $ \x -> a x & a & a & a
            , bench "streaming" $ whnf drainS $ \x -> s x & s & s & s
            , bench "pipes"     $ whnf drainP $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnf drainC $ c C.=$= c C.=$= c C.=$= c
            ]

        , let m = M.filtered (<= value)
              a = A.filter (<= value)
              s = S.filter (<= value)
              p = P.filter (<= value)
              c = C.filter (<= value)
          in bgroup "passing-filters"
            [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
            , bench "asyncly"   $ whnf drainA $ \x -> a x & a & a & a
            , bench "streaming" $ whnf drainS $ \x -> s x & s & s & s
            , bench "pipes"     $ whnf drainP $ p P.>-> p P.>-> p P.>-> p
            , bench "conduit"   $ whnf drainC $ c C.=$= c C.=$= c C.=$= c
            ]

        , let m = M.filtered (> value)
              a = A.filter   (> value)
              s = S.filter   (> value)
              p = P.filter   (> value)
              c = C.filter   (> value)
          in bgroup "blocking-filters"
            [ bench "machines"  $ whnf drainM $ m M.~> m M.~> m M.~> m
            , bench "asyncly"   $ whnf drainA $ \x -> a x & a & a & a
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
        , let f = A.filter (<= value)
          in bgroup "asyncly-filters"
            [ bench "1" $ whnf drainA (\x -> f x)
            , bench "2" $ whnf drainA $ \x -> f x & f
            , bench "3" $ whnf drainA $ \x -> f x & f & f
            , bench "4" $ whnf drainA $ \x -> f x & f & f & f
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
        , let f = A.filter (<= value) . fmap (subtract 1)
          in bgroup "asyncly-map-filter"
            [ bench "1" $ whnf drainA (\x -> f x)
            , bench "2" $ whnf drainA $ \x -> f x & f
            , bench "3" $ whnf drainA $ \x -> f x & f & f
            , bench "4" $ whnf drainA $ \x -> f x & f & f & f
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
        , let f = A.filter (> value)
          in bgroup "asyncly-blocking-filter"
            [ bench "1" $ whnf drainA (\x -> f x)
            , bench "2" $ whnf drainA $ \x -> f x & f
            , bench "3" $ whnf drainA $ \x -> f x & f & f
            , bench "4" $ whnf drainA $ \x -> f x & f & f & f
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
        -}
  ]
