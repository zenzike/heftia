{-# LANGUAGE  TemplateHaskell #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

-- Benchmarking effect invocation and monadic bind
module BenchCountdown where

import Control.Carrier.Reader qualified as F
import Control.Carrier.State.Strict qualified as F
import Control.Ev.Eff qualified as E
import Control.Ev.Util qualified as E
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Freer.State qualified as FS
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Monad.Hefty.State qualified as H
import Control.Monad.Identity qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State.Strict qualified as M
import Effectful qualified as EL
import Effectful.Reader.Dynamic qualified as EL
import Effectful.State.Dynamic qualified as EL
import Polysemy qualified as P
import Polysemy.Reader qualified as P
import Polysemy.State qualified as P
import "eff" Control.Effect qualified as EF
import "effective" Control.Effect qualified as EV
import "effective" Control.Effect.State qualified as EV
import "effective" Control.Effect.Reader qualified as EV
import "effective" Control.Effect.CodeGen qualified as EV
import EffectiveStaged as EVstaged
import "effective" Control.Effect.Internal.AlgTrans qualified as EV
import Data.Functor.Identity

programHeftia :: (H.Member (H.State Int) es) => H.Eff '[] es Int
programHeftia = do
    x <- H.get @Int
    if x == 0
        then pure x
        else do
            H.put (x - 1)
            programHeftia

countdownHeftia :: Int -> (Int, Int)
countdownHeftia n = H.runPure $ H.runState n programHeftia

countdownHeftiaDeep :: Int -> (Int, Int)
countdownHeftiaDeep n = H.runPure $ runR $ runR $ runR $ runR $ runR $ H.runState n $ runR $ runR $ runR $ runR $ runR $ programHeftia
  where
    runR = H.runAsk ()

countdownHeftiaNaive :: Int -> (Int, Int)
countdownHeftiaNaive n = H.runPure $ H.runStateNaive n programHeftia

countdownHeftiaNaiveDeep :: Int -> (Int, Int)
countdownHeftiaNaiveDeep n = H.runPure $ runR $ runR $ runR $ runR $ runR $ H.runStateNaive n $ runR $ runR $ runR $ runR $ runR $ programHeftia
  where
    runR = H.runAsk ()

programFreer :: (FS.Member (FS.State Int) es) => FS.Eff es Int
programFreer = do
    x <- FS.get @Int
    if x == 0
        then pure x
        else do
            FS.put (x - 1)
            programFreer

countdownFreer :: Int -> (Int, Int)
countdownFreer n = FS.run $ FS.runState n programFreer

countdownFreerDeep :: Int -> (Int, Int)
countdownFreerDeep n = FS.run $ runR $ runR $ runR $ runR $ runR $ FS.runState n $ runR $ runR $ runR $ runR $ runR $ programFreer
  where
    runR = FS.runReader ()

programSem :: (P.Member (P.State Int) es) => P.Sem es Int
programSem = do
    x <- P.get @Int
    if x == 0
        then pure x
        else do
            P.put (x - 1)
            programSem

countdownSem :: Int -> (Int, Int)
countdownSem n = P.run $ P.runState n programSem

countdownSemDeep :: Int -> (Int, Int)
countdownSemDeep n = P.run $ runR $ runR $ runR $ runR $ runR $ P.runState n $ runR $ runR $ runR $ runR $ runR $ programSem
  where
    runR = P.runReader ()

programFused :: (F.Has (F.State Int) sig m) => m Int
programFused = do
    x <- F.get @Int
    if x == 0
        then pure x
        else do
            F.put (x - 1)
            programFused

countdownFused :: Int -> (Int, Int)
countdownFused n = F.run $ F.runState n programFused

countdownFusedDeep :: Int -> (Int, Int)
countdownFusedDeep n = F.run $ runR $ runR $ runR $ runR $ runR $ F.runState n $ runR $ runR $ runR $ runR $ runR $ programFused
  where
    runR = F.runReader ()

programEffectful :: (EL.State Int EL.:> es) => EL.Eff es Int
programEffectful = do
    x <- EL.get @Int
    if x == 0
        then pure x
        else do
            EL.put (x - 1)
            programEffectful

countdownEffectful :: Int -> (Int, Int)
countdownEffectful n = EL.runPureEff $ EL.runStateLocal n programEffectful

countdownEffectfulDeep :: Int -> (Int, Int)
countdownEffectfulDeep n =
    EL.runPureEff $ runR $ runR $ runR $ runR $ runR $ EL.runStateLocal n $ runR $ runR $ runR $ runR $ runR $ programEffectful
  where
    runR = EL.runReader ()

programEff :: (EF.State Int EF.:< es) => EF.Eff es Int
programEff = do
    x <- EF.get @Int
    if x == 0
        then pure x
        else do
            EF.put (x - 1)
            programEff

countdownEff :: Int -> (Int, Int)
countdownEff n = EF.run $ EF.runState n programEff

countdownEffDeep :: Int -> (Int, Int)
countdownEffDeep n = EF.run $ runR $ runR $ runR $ runR $ runR $ EF.runState n $ runR $ runR $ runR $ runR $ runR $ programEff
  where
    runR = EF.runReader ()

programEv :: (E.State Int E.:? es) => E.Eff es Int
programEv = do
    x <- E.perform (E.get @Int) ()
    if x == 0
        then pure x
        else do
            E.perform E.put (x - 1)
            programEv

countdownEv :: Int -> (Int, Int)
countdownEv n = E.runEff $ runStateEv n programEv

countdownEvDeep :: Int -> (Int, Int)
countdownEvDeep n = E.runEff $ runR $ runR $ runR $ runR $ runR $ runStateEv n $ runR $ runR $ runR $ runR $ runR $ programEv
  where
    runR = E.reader ()

runStateEv :: s -> E.Eff (E.State s E.:* es) a -> E.Eff es (s, a)
runStateEv s0 m = E.state s0 do
    r <- m
    s <- E.perform E.get ()
    pure (s, r)

programMtl :: (M.MonadState Int m) => m Int
programMtl = do
    x <- M.get @Int
    if x == 0
        then pure x
        else do
            M.put (x - 1)
            programMtl

countdownMtl :: Int -> (Int, Int)
countdownMtl = M.runState programMtl

countdownMtlDeep :: Int -> (Int, Int)
countdownMtlDeep n = M.runIdentity $ runR $ runR $ runR $ runR $ runR $ M.runStateT (runR $ runR $ runR $ runR $ runR $ programMtl) n
  where
    runR = (`M.runReaderT` ())

programEffective :: Int EV.! '[EV.Get Int, EV.Put Int]
programEffective = do
    x <- EV.get @Int
    if x == 0
        then pure x
        else do
            EV.put (x - 1)
            programEffective

countdownEffective :: Int -> (Int, Int)
countdownEffective s = EV.handle (EV.state s) programEffective

countdownEffectiveDeep :: Int -> (Int, Int)
countdownEffectiveDeep s = EV.handle (run EV.|> run EV.|> run EV.|> run EV.|> run EV.|> EV.state s EV.|>
                                      run EV.|> run EV.|> run EV.|> run EV.|> run ) programEffective
  where run = EV.reader ()

countdownEffectiveStaged :: Int -> (Int, Int)
countdownEffectiveStaged n = runIdentity (EV.runStateT p n) where 
  p :: EV.StateT Int Identity Int
  p = $$(EV.stage
    (EV.upState @Int @Identity `EV.fuseAT` EV.stateAT @(EV.Up Int))
    (EVstaged.countdownGen [||p||]))
    

countdownEffectiveDeepStaged :: Int -> (Int, Int)
countdownEffectiveDeepStaged n = 
  (runIdentity . r . r . r . r . r . (flip EV.runStateT n) . r . r . r . r . r) p 
  where 
    r :: EV.ReaderT () m a -> m a
    r m = EV.runReaderT m () 

    p :: EVstaged.R5 (EV.StateT Int (EVstaged.R5 Identity)) Int
    p = $$(EV.stage
      (             EVstaged.upR5 @(EV.StateT Int (EVstaged.R5 Identity))
        `EV.fuseAT` EV.upState @Int @(EVstaged.R5 Identity)
        `EV.fuseAT` EVstaged.upR5 @Identity 
        ---------------------
        `EV.fuseAT` EV.weakenC @((~) EV.Gen) (
                    EVstaged.r5AT
        `EV.fuseAT` EV.stateAT @(EV.Up Int)
        `EV.fuseAT` EVstaged.r5AT ))
      (EVstaged.countdownGen [||p||]))