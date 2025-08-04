{-# LANGUAGE TemplateHaskell #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

-- Benchmarking yield-intensive code
module BenchPyth where

import Control.Algebra qualified as F
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Carrier.NonDet.Church qualified as F
import Control.Carrier.Reader qualified as F
import Control.Ev.Eff qualified as E
import Control.Ev.Util qualified as E
import Control.Monad (MonadPlus)
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.NonDet qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.NonDet qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Monad.Identity qualified as M
import Control.Monad.Logic qualified as M
import Control.Monad.Reader qualified as M
import Control.Mp.Eff qualified as Mp
import Control.Mp.Util qualified as Mp
import "eff" Control.Effect qualified as EF
import "effective" Control.Effect qualified as EV
import "effective" Control.Effect.Alternative qualified as EV
import "effective" Control.Effect.Nondet qualified as EV
import "effective" Control.Effect.Nondet.Logic qualified as EVL
import "effective" Control.Effect.Reader qualified as EV
import "effective" Data.HFunctor qualified as EV
import "effective" Control.Effect.CodeGen qualified as EV
import "effective" Control.Monad.Trans.List qualified as EV
import "effective" Control.Effect.Internal.AlgTrans qualified as EV
import EffectiveStaged as EVstaged
import Data.Functor.Identity
import Data.Proxy

programFreer :: (FS.Member FS.NonDet es) => Int -> FS.Eff es (Int, Int, Int)
programFreer upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n

pythFreer :: Int -> [(Int, Int, Int)]
pythFreer n = FS.run $ FS.makeChoiceA $ programFreer n

pythFreerDeep :: Int -> [(Int, Int, Int)]
pythFreerDeep n = FS.run $ run $ run $ run $ run $ run $ FS.makeChoiceA $ run $ run $ run $ run $ run $ programFreer n
  where
    run = FS.runReader ()

programHeftia :: (H.Member H.Choose es, H.Member H.Empty es) => Int -> H.Eff '[] es (Int, Int, Int)
programHeftia upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else H.empty
  where
    choice 0 = H.empty
    choice n = choice (n - 1) `H.branch` pure n

pythHeftia :: Int -> [(Int, Int, Int)]
pythHeftia n = H.runPure $ H.runNonDet $ programHeftia n

pythHeftiaDeep :: Int -> [(Int, Int, Int)]
pythHeftiaDeep n = H.runPure $ run $ run $ run $ run $ run $ H.runNonDet $ run $ run $ run $ run $ run $ programHeftia n
  where
    run = H.runAsk ()

programFused :: (Monad m, Alternative m) => Int -> m (Int, Int, Int)
programFused upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice x = F.oneOf [1 .. x]

pythFused :: Int -> [(Int, Int, Int)]
pythFused n = F.run $ F.runNonDetA $ programFused n

pythFusedDeep :: Int -> [(Int, Int, Int)]
pythFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runNonDetA $ run $ run $ run $ run $ run $ programFused n
  where
    run = F.runReader ()

programEv :: (E.Choose E.:? e) => Int -> E.Eff e (Int, Int, Int)
programEv upbound = do
    x <- E.perform E.choose upbound
    y <- E.perform E.choose upbound
    z <- E.perform E.choose upbound
    if x * x + y * y == z * z then return (x, y, z) else E.perform (\r -> E.none r) ()

pythEv :: Int -> [(Int, Int, Int)]
pythEv n = E.runEff $ E.chooseAll $ programEv n

pythEvDeep :: Int -> [(Int, Int, Int)]
pythEvDeep n = E.runEff $ run $ run $ run $ run $ run $ E.chooseAll $ run $ run $ run $ run $ run $ programEv n
  where
    run = E.reader ()

programMp :: (Mp.Choose Mp.:? e) => Int -> Mp.Eff e (Int, Int, Int)
programMp upbound = do
    x <- Mp.perform Mp.choose upbound
    y <- Mp.perform Mp.choose upbound
    z <- Mp.perform Mp.choose upbound
    if x * x + y * y == z * z then return (x, y, z) else Mp.perform (\r -> Mp.none r) ()

pythMp :: Int -> [(Int, Int, Int)]
pythMp n = Mp.runEff $ Mp.chooseAll $ programMp n

pythMpDeep :: Int -> [(Int, Int, Int)]
pythMpDeep n = Mp.runEff $ run $ run $ run $ run $ run $ Mp.chooseAll $ run $ run $ run $ run $ run $ programMp n
  where
    run = Mp.reader ()

programEff :: (EF.NonDet EF.:< es) => Int -> EF.Eff es (Int, Int, Int)
programEff upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n

pythEff :: Int -> [(Int, Int, Int)]
pythEff n = EF.run $ EF.runNonDetAll $ programEff n

pythEffDeep :: Int -> [(Int, Int, Int)]
pythEffDeep n = EF.run $ run $ run $ run $ run $ run $ EF.runNonDetAll $ run $ run $ run $ run $ run $ programEff n
  where
    run = EF.runReader ()

programMtl :: (MonadPlus m) => Int -> m (Int, Int, Int)
programMtl upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n

pythLogict :: Int -> [(Int, Int, Int)]
pythLogict n = M.observeAll $ programMtl n

pythLogictDeep :: Int -> [(Int, Int, Int)]
pythLogictDeep n = M.runIdentity $ runR $ runR $ runR $ runR $ runR $ M.observeAllT $ runR $ runR $ runR $ runR $ runR $ programMtl n
  where
    runR = (`M.runReaderT` ())

programEffective :: Int -> (Int, Int, Int)  EV.! '[EV.Empty, EV.Choose]
programEffective upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n

pythEffective :: Int -> [(Int, Int, Int)]
pythEffective n = EV.handle (EV.unscope (Proxy @(EV.Choose_)) EV.|> EV.nondet) (programEffective n)

pythEffectiveBacktrack :: Int -> [(Int, Int, Int)]
pythEffectiveBacktrack n = EV.handle EV.backtrack (programEffective n)

pythEffective' :: Int -> [(Int, Int, Int)]
pythEffective' n = EV.handle EV.nondet' (programEffective n)

pythEffectiveDeep :: Int -> [(Int, Int, Int)]
pythEffectiveDeep n = EV.handle h p
  where
    h :: EV.Handler '[EV.Choose, EV.Ask (), EV.Local (), EV.Empty, EV.Nondet ]
      '[]
      '[ EV.ReaderT (), EV.ReaderT (), EV.ReaderT (), EV.ReaderT (), EV.ReaderT ()
       , EV.ListT
       , EV.ReaderT (), EV.ReaderT (), EV.ReaderT (), EV.ReaderT (), EV.ReaderT ()]
       '[[]]
    h = (EV.unscope (Proxy @EV.Choose_) EV.|>
        run EV.|> run EV.|> run EV.|> run EV.|> run EV.|>
        EV.nondet EV.|>
        run EV.|> run EV.|> run EV.|> run EV.|> run )
    run = EV.reader ()
    p = (programEffective n)

pythEffectiveDeep' :: Int -> [(Int, Int, Int)]
pythEffectiveDeep' n = EV.handle (EV.unscope (Proxy @(EV.Choose_)) EV.|> run EV.|> run EV.|> run EV.|> run EV.|> run EV.|> EV.nondet EV.|>
                                  run EV.|> run EV.|> run EV.|> run EV.|> run ) p
  where
    run = EV.asker ()
    p = (programEffective n)

pythEffectiveDeep'' :: Int -> [(Int, Int, Int)]
pythEffectiveDeep'' n = extract
                      $ ha . ha . ha . ha . ha
                      . hn
                      . ha . ha . ha.  ha . ha . hu $ p
  where
    run = EV.asker ()
    p = (programEffective n)

    ha :: forall effs a . EV.HFunctor (EV.Effs effs) => EV.Prog (EV.Ask () ': effs) a -> EV.Prog effs a
    ha = EV.handlePApp @'[EV.Ask ()] @'[] @effs run

    hn = EV.handlePApp EV.nondet

    hu = EV.handlePApp (EV.unscope (Proxy @(EV.Choose_)))


    extract :: EV.Prog '[] a -> a
    extract = M.runIdentity . EV.eval EV.absurdEffs

pythEffectiveList :: Int -> [(Int, Int, Int)]
pythEffectiveList n = EV.handle EV.list (programEffective n)

pythEffectiveLogic :: Int -> [(Int, Int, Int)]
pythEffectiveLogic n = EV.handle EVL.list (programEffective n)

pythNative :: Int -> [(Int, Int, Int)]
pythNative n = [ (x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x * x + y * y == z * z ]

choose :: Int -> [Int]
choose n = $$(EV.stage (EV.pushWithUpAT @Identity)
  (EVstaged.chooseGen [||n||] [||choose||]))

pythEffectiveStaged :: Int -> [(Int, Int, Int)]
pythEffectiveStaged n = $$(EV.stage (EV.pushWithUpAT @Identity)
 (EVstaged.pythGen [||n||] [||choose||]))

pythEffectiveDeepStaged :: Int -> [(Int, Int, Int)]
pythEffectiveDeepStaged n = (runIdentity . r . r . r . r . r . EV.runListT' . r . r . r . r . r)
  $$(EV.stage
         (r5AT
            `EV.fuseAT` EV.pushWithUpAT @(EVstaged.R5 Identity)
            `EV.fuseAT` upR5 @Identity
            `EV.fuseAT` EV.weakenC @((~) EV.Gen) r5AT)
       (EVstaged.pythGen [||n||] [||choose||]))
  where
    r :: EV.ReaderT () m a -> m a
    r m = EV.runReaderT m ()