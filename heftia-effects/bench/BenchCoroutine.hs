{-# LANGUAGE  TemplateHaskell #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

module BenchCoroutine where

import Control.Monad (forM)
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.Coroutine qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.Coroutine qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Mp.Eff qualified as Mp
import Control.Mp.Util qualified as Mp
import "eff" Control.Effect qualified as E
import "effective" Control.Effect qualified as EV
import "effective" Control.Effect.CodeGen qualified as EV
import "effective" Control.Effect.Yield qualified as EV
import "effective" Control.Monad.Trans.YRes qualified as EV hiding (yield)
import "effective" Control.Monad.Trans.YRes qualified as EVY
import EffectiveStaged as EVstaged
import "effective" Control.Effect.Internal.AlgTrans qualified as EV
import Data.Functor.Identity

{-
In the original code of heftia, `forM` is used in the testing programs:

> forM :: Applicative m => [a] -> (a -> m b) -> m [b]
> forM [] f = []
> forM (x:xs) f = liftA2 (:) (f x) (forM xs f)

Note that this is not tail recursive, and when `liftA2` has linear time complexity,
the overall time complexity is quadratic. This is unfornately the case for the 
naive implementations of free monad and the resumption monad transformer. 
Libraries such as freer and heftia avoided this problem because they happen to
used the efficient FTCQueue-based implementation in place of the naive
implementation. We can do the same to optimise our resumption monad, but for
now, I will just 'correct' the benchmarking code by replacing `forM` with the
following tail-recursive `forM'`, as I think it is reasonable to expect
coroutines to be tail-recursive.
-}
forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' xs f = go [] xs where
  go rs []     = pure (reverse rs)
  go rs (x:xs) = do r <- f x; go (r:rs) xs

programFreer :: (FS.Member (FS.Yield Int Int) es) => Int -> FS.Eff es [Int]
programFreer upbound =
    forM' [1 .. upbound] (`FS.yield` id)

loopStatusFreer :: FS.Status es Int Int r -> FS.Eff es r
loopStatusFreer = \case
    FS.Done r -> pure r
    FS.Continue i f -> loopStatusFreer =<< f (i + 100)

coroutineFreer :: Int -> [Int]
coroutineFreer n = FS.run $ loopStatusFreer =<< FS.runC (programFreer n)

coroutineFreerDeep :: Int -> [Int]
coroutineFreerDeep n = FS.run $ run $ run $ run $ run $ run $ loopStatusFreer =<< FS.runC (run $ run $ run $ run $ run $ programFreer n)
  where
    run = FS.runReader ()

programHeftia :: (H.Member (H.Yield Int Int) es) => Int -> H.Eff '[] es [Int]
programHeftia upbound =
    forM' [1 .. upbound] H.yield

loopStatusHeftia :: H.Status (H.Eff '[] ef) Int Int r -> H.Eff '[] ef r
loopStatusHeftia = \case
    H.Done r -> pure r
    H.Continue i f -> loopStatusHeftia =<< f (i + 100)

coroutineHeftia :: Int -> [Int]
coroutineHeftia n = H.runPure $ loopStatusHeftia =<< H.runCoroutine (programHeftia n)

coroutineHeftiaDeep :: Int -> [Int]
coroutineHeftiaDeep n = H.runPure $ run $ run $ run $ run $ run $ loopStatusHeftia =<< H.runCoroutine (run $ run $ run $ run $ run $ programHeftia n)
  where
    run = H.runAsk ()

programEff :: (E.Coroutine Int Int E.:< es) => Int -> E.Eff es [Int]
programEff upbound =
    forM' [1 .. upbound] $ E.yield @Int @Int

loopStatusEff :: E.Status es Int Int r -> E.Eff es r
loopStatusEff = \case
    E.Done r -> pure r
    E.Yielded i f -> loopStatusEff =<< E.runCoroutine (f (i + 100))

coroutineEff :: Int -> [Int]
coroutineEff n = E.run $ loopStatusEff =<< E.runCoroutine (programEff n)

coroutineEffDeep :: Int -> [Int]
coroutineEffDeep n = E.run $ run $ run $ run $ run $ run $ loopStatusEff =<< E.runCoroutine (run $ run $ run $ run $ run $ programEff n)
  where
    run = E.runReader ()

programMp :: (MpYield Int Int Mp.:? e) => Int -> Mp.Eff e [Int]
programMp n = forM' [0 .. n] $ \i -> Mp.perform mpYield i

loopStatusMp :: H.Status (Mp.Eff e) Int Int r -> Mp.Eff e r
loopStatusMp = \case
    H.Done r -> pure r
    H.Continue a k -> loopStatusMp =<< k (a + 100)

coroutineMp :: Int -> [Int]
coroutineMp n = Mp.runEff $ loopStatusMp =<< mpCoroutine @Int @Int (programMp n)

coroutineMpDeep :: Int -> [Int]
coroutineMpDeep n = Mp.runEff $ run $ run $ run $ run $ run $ loopStatusMp =<< mpCoroutine @Int @Int (run $ run $ run $ run $ run $ programMp n)
  where
    run = Mp.reader ()

newtype MpYield a b e ans = MpYield {mpYield :: Mp.Op a b e ans}

mpCoroutine :: Mp.Eff (MpYield a b Mp.:* e) r -> Mp.Eff e (H.Status (Mp.Eff e) a b r)
mpCoroutine = Mp.handler MpYield {mpYield = Mp.operation $ \a k -> pure $ H.Continue a k} . fmap H.Done

-- Coroutine of effective has a slightly different API from the other libraries. 
-- Instead of having a hand-written looper funciton that interact with a coroutine,
-- the API of `effective` takes two coroutines and let them interact.
-- I think this is a more natural API although it makes effective slower than
-- the other libraries in this test case.

program1Effective :: (EV.Member (EV.Yield Int Int) sig) => Int -> EV.Prog sig [Int] 
program1Effective n = forM' [0 .. n] $ \i -> EV.yield i

program2Effective :: (EV.Member (EV.Yield Int Int) sig) => Int -> EV.Prog sig [Int] 
program2Effective a = EV.yield (a + 100) >>= program2Effective

coroutineEffective :: Int -> [Int]
coroutineEffective n = either id id $ 
  EV.handle (EV.pingpongWith @'[] @Int @Int program2Effective) (program1Effective n)

coroutineEffectiveStaged :: Int -> [Int]
coroutineEffectiveStaged n = either id id $ runIdentity $ EV.pingpong (coroutine1 n) coroutine2 where
  coroutine1 :: Int -> EV.YResT Int Int Identity [Int]
  coroutine1 upbound = go [] [1 .. upbound] where
    go :: [Int] -> [Int] -> EV.YResT Int Int Identity [Int]
    go rs ns = $$(EV.stage 
       (EV.upCache @(EV.YResT Int Int Identity) `EV.fuseAT` EV.yResUpAT @Identity @Int @Int) 
       (EVstaged.coroutine1Gen [||rs||] [||ns||] [||go||]))
       
  coroutine2 :: Int -> EV.YResT Int Int Identity a
  coroutine2 a = $$(EV.stage 
       (EV.upCache @(EV.YResT Int Int Identity) `EV.fuseAT` EV.yResUpAT @Identity @Int @Int) 
       (EVstaged.coroutine2Gen [||a||] [||coroutine2||]))