-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

module Main where

import BenchCatch
import BenchCoroutine
import BenchCountdown
import BenchLocal
import BenchPyth
import Data.Functor ((<&>))
import Test.Tasty.Bench

main :: IO ()
main =
    defaultMain
        [ bgroup "countdown.shallow" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf countdownHeftia x
                    , bench "freer" $ nf countdownFreer x
                    , bench "polyemy" $ nf countdownSem x
                    , bench "fused" $ nf countdownFused x
                    , bench "effectful" $ nf countdownEffectful x
                    , bench "eff" $ nf countdownEff x
                    , bench "ev" $ nf countdownEv x
                    , bench "mtl" $ nf countdownMtl x
                    , bench "effective" $ nf countdownEffective x
                    ]
        , bgroup "countdown.deep" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5" $ nf countdownHeftiaDeep x
                    , bench "freer.5+5" $ nf countdownFreerDeep x
                    , bench "polysemy.5+5" $ nf countdownSemDeep x
                    , bench "fused.5+5" $ nf countdownFusedDeep x
                    , bench "effectful.5+5" $ nf countdownEffectfulDeep x
                    , bench "eff.5+5" $ nf countdownEffDeep x
                    , bench "ev.5+5" $ nf countdownEvDeep x
                    , bench "mtl.5+5" $ nf countdownMtlDeep x
                    , bench "effective.5+5" $ nf countdownEffectiveDeep x
                    ]
        , bgroup "catch.shallow" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf catchHeftia x
                    , bench "polysemy" $ nf catchSem x
                    , bench "fused" $ nf catchFused x
                    , bench "effectful" $ nf catchEffectful x
                    , -- , bench "eff" $ nf catchEff x
                      -- `eff` is x500 slow in this case, so it is excluded because it makes the graph hard to read.
                      bench "mtl" $ nf catchMtl x
                    , bench "effective" $ nf catchEffective x
                    ]
        , bgroup "catch.deep" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5+0" $ nf catchHeftiaDeep0 x
                    , bench "heftia.5+4+1" $ nf catchHeftiaDeep1 x
                    , bench "heftia.5+3+2" $ nf catchHeftiaDeep2 x
                    , bench "heftia.5+2+3" $ nf catchHeftiaDeep3 x
                    , bench "heftia.5+1+4" $ nf catchHeftiaDeep4 x
                    , bench "heftia.5+0+5" $ nf catchHeftiaDeep5 x
                    , bench "polysemy.5+5" $ nf catchSemDeep x
                    , bench "fused.5+5" $ nf catchFusedDeep x
                    , bench "effectful.5+5" $ nf catchEffectfulDeep x
                    , -- , bench "eff.5+5" $ nf catchEffDeep x
                      bench "mtl.5+5" $ nf catchMtlDeep x
                    , bench "effective.5+5" $ nf catchEffectiveDeep x
                    ]
        , bgroup "local.shallow" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf localHeftia x
                    , bench "polysemy" $ nf localSem x
                    , bench "fused" $ nf localFused x
                    , bench "effectful" $ nf localEffectful x
                    -- , bench "eff" $ nf localEff x
                    -- `eff` is x500 slow in this case, so it is excluded because it makes the graph hard to read.
                    --  bench "mtl" $ nf localMtl x
                    , bench "effective" $ nf localEffective x
                    ]
        , bgroup "local.deep" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5+0" $ nf localHeftiaDeep0 x
                    , bench "heftia.5+4+1" $ nf localHeftiaDeep1 x
                    , bench "heftia.5+3+2" $ nf localHeftiaDeep2 x
                    , bench "heftia.5+2+3" $ nf localHeftiaDeep3 x
                    , bench "heftia.5+1+4" $ nf localHeftiaDeep4 x
                    , bench "heftia.5+0+5" $ nf localHeftiaDeep5 x
                    , bench "polysemy.5+5" $ nf localSemDeep x
                    , bench "fused.5+5" $ nf localFusedDeep x
                    , bench "effectful.5+5" $ nf localEffectfulDeep x
                    --  bench "eff.5+5" $ nf localEffDeep x
                    --  bench "mtl.5+5" $ nf localMtlDeep x
                    , bench "effective.5+5" $ nf localEffectiveDeep x
                    ]
        , bgroup "nondet.shallow" $
            [32] <&> \x ->
                bgroup
                    (show (x, (pythEff x) == (pythNative x)))
                    [ bench "heftia" $ nf pythHeftia x
                    , bench "freer" $ nf pythFreer x
                    , bench "fused" $ nf pythFused x
                    , bench "ev" $ nf pythEv x
                    , bench "mp" $ nf pythMp x
                    , bench "eff" $ nf pythEff x
                    , bench "mtl-logict" $ nf pythLogict x
                    , bench "effective" $ nf pythEffective x
                    , bench "effective.staged" $ nf pythEffectiveStaged x
                    , bench "native" $ nf pythNative x
                    ] -- Polysemy case is excluded because of incorrect semantics.
        , bgroup "nondet.deep" $
            [32] <&> \x ->
                bgroup
                    (show (x, (pythEffDeep x) == (pythEffectiveDeep x)))
                    [ bench "heftia.5+5" $ nf pythHeftiaDeep x
                    , bench "freer.5+5" $ nf pythFreerDeep x
                    , bench "fused.5+5" $ nf pythFusedDeep x
                    , bench "ev.5+5" $ nf pythEvDeep x
                    , bench "mp.5+5" $ nf pythMpDeep x
                    , bench "eff.5+5" $ nf pythEffDeep x
                    , bench "mtl-logict.5+5" $ nf pythLogictDeep x
                    , bench "effective.5+5" $ nf pythEffectiveDeep x
--                    , bench "effective.ignore.5+5" $ nf pythEffectiveDeep' x
--                    , bench "effective.ignore'.5+5" $ nf pythEffectiveDeep'' x
                    , bench "effective.staged.5+5" $ nf pythEffectiveDeepStaged x
                    ]
        , bgroup "coroutine.shallow" $
            [1000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf coroutineHeftia x
                    , bench "freer" $ nf coroutineFreer x
                    , bench "eff" $ nf coroutineEff x
                    , bench "mp" $ nf coroutineMp x
                    -- `mpeff` is O(n^2) slow because of: https://dl.acm.org/doi/10.1145/2633357.2633360
                    -- `eff` is probably for the same reason.
                    ] -- add mtl?
        , bgroup "coroutine.deep" $
            [1000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5" $ nf coroutineHeftiaDeep x
                    , bench "freer.5+5" $ nf coroutineFreerDeep x
                    , bench "eff.5+5" $ nf coroutineEffDeep x
                    , bench "mp.5+5" $ nf coroutineMpDeep x
                    ]
        ]
