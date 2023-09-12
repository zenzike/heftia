{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Heftia where

import Control.Applicative (Alternative)
import Control.Arrow ((>>>))
import Control.Effect.Class (
    EffectDataHandler,
    EffectsVia (EffectsVia),
    LiftIns (LiftIns),
    SendIns,
    SendSig,
    Signature,
    runEffectsVia,
    sendIns,
    sendSig,
    type (~>),
 )
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Effect.Freer (FreerEffects, freerEffects, interpose, runFreerEffects)
import Control.Freer.Trans (TransFreer, interpretFT, liftInsT, liftLowerFT)
import Control.Heftia.Trans (
    TransHeftia,
    elaborateHT,
    hoistHeftia,
    interpretLowerH,
    liftLowerHT,
    liftSigT,
    reelaborateHT,
    runElaborateH,
    transformHT,
    translateT,
 )
import Control.Monad (MonadPlus)
import Control.Monad.Cont (ContT (ContT), MonadTrans, runContT)
import Control.Monad.Trans.Heftia (MonadTransHeftia, elaborateMK, elaborateMT)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT)
import Data.Free.Union (Member, Union, project)
import Data.Hefty.Sum (SumUnionH)
import Data.Hefty.Union (
    IsMemberH,
    MemberH,
    UnionH (
        absurdUnionH,
        bundleUnion2H,
        bundleUnion3H,
        bundleUnion4H,
        decompH,
        flipUnion3H,
        flipUnionH,
        flipUnionUnderH,
        inject0H,
        injectH,
        projectH,
        rot3H,
        rot3H',
        unbundleUnion2H,
        unbundleUnion3H,
        unbundleUnion4H,
        weaken2H,
        weaken2Under2H,
        weaken2UnderH,
        weaken3H,
        weaken3UnderH,
        weaken4H,
        weakenH,
        weakenUnder2H,
        weakenUnder3H,
        weakenUnderH
    ),
 )
import Data.Kind (Type)

newtype
    HeftiaUnion
        (h :: Signature -> (Type -> Type) -> Type -> Type)
        u
        (es :: [Signature])
        f
        a = HeftiaUnion {runHeftiaUnion :: h (u es) f a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)
    deriving stock (Foldable, Traversable)

type HeftiaEffects h u es f = EffectsVia EffectDataHandler (HeftiaUnion h u es f)

runHeftiaEffects :: HeftiaEffects h u es f ~> h (u es) f
runHeftiaEffects = runHeftiaUnion . runEffectsVia
{-# INLINE runHeftiaEffects #-}

heftiaEffects :: h (u es) f ~> HeftiaEffects h u es f
heftiaEffects = EffectsVia . HeftiaUnion
{-# INLINE heftiaEffects #-}

newtype HeftiaUnionForSendIns handleHere h u es f a = HeftiaUnionForSendIns
    {runHeftiaUnionForSendIns :: HeftiaUnion h u es f a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)
    deriving stock (Foldable, Traversable)

instance
    SendIns e (HeftiaUnionForSendIns (LiftIns e `IsMemberH` es) h u es f) =>
    SendIns e (HeftiaUnion h u es f)
    where
    sendIns = runHeftiaUnionForSendIns @(LiftIns e `IsMemberH` es) . sendIns
    {-# INLINE sendIns #-}

instance
    (TransHeftia c h, UnionH u, MemberH u (LiftIns e) es, HFunctor (u es)) =>
    SendIns e (HeftiaUnionForSendIns 'True h u es f)
    where
    sendIns = HeftiaUnionForSendIns . HeftiaUnion . liftSigT . injectH . LiftIns
    {-# INLINE sendIns #-}

instance
    (TransHeftia c h, SendIns e f, c f, HFunctor (u es)) =>
    SendIns e (HeftiaUnionForSendIns 'False h u es f)
    where
    sendIns = HeftiaUnionForSendIns . HeftiaUnion . liftLowerHT . sendIns
    {-# INLINE sendIns #-}

instance
    (TransHeftia c h, UnionH u, MemberH u e es, HFunctor (u es)) =>
    SendSig e (HeftiaUnion h u es f)
    where
    sendSig = HeftiaUnion . liftSigT . hfmap runHeftiaUnion . injectH
    {-# INLINE sendSig #-}

runElaborate ::
    (TransHeftia c h, HFunctor (u es), c f, UnionH u) =>
    (u es f ~> f) ->
    HeftiaEffects h u es f ~> f
runElaborate f = runElaborateH f . runHeftiaEffects
{-# INLINE runElaborate #-}

runElaborateK ::
    (MonadTransHeftia h, HFunctor (u es), UnionH u, Monad m) =>
    (a -> m r) ->
    (forall x. (x -> m r) -> u es (ContT r m) x -> m r) ->
    HeftiaEffects h u es m a ->
    m r
runElaborateK k f = (`runContT` k) . runElaborateContT \e -> ContT (`f` e)
{-# INLINE runElaborateK #-}

runElaborateContT ::
    (MonadTransHeftia h, HFunctor (u es), UnionH u, Monad m) =>
    (u es (ContT r m) ~> ContT r m) ->
    HeftiaEffects h u es m ~> ContT r m
runElaborateContT f = elaborateMK f . runHeftiaEffects
{-# INLINE runElaborateContT #-}

runElaborateT ::
    (MonadTransHeftia h, HFunctor (u es), UnionH u, MonadTrans t, Monad m, Monad (t m)) =>
    (u es (t m) ~> t m) ->
    HeftiaEffects h u es m ~> t m
runElaborateT f = elaborateMT f . runHeftiaEffects
{-# INLINE runElaborateT #-}

elaborate ::
    (TransHeftia c h, HFunctor (u es), c f, UnionH u, c g) =>
    (f ~> g) ->
    (u es g ~> g) ->
    HeftiaEffects h u es f ~> g
elaborate f g = elaborateHT f g . runHeftiaEffects
{-# INLINE elaborate #-}

interpretH ::
    (TransHeftia c h, UnionH u, HFunctor (u es), HFunctor (u (e : es)), HFunctor e, c f) =>
    (e (HeftiaEffects h u es f) ~> HeftiaEffects h u es f) ->
    HeftiaEffects h u (e ': es) f ~> HeftiaEffects h u es f
interpretH i a =
    heftiaEffects $ ($ runHeftiaEffects a) $ elaborateHT liftLowerHT \u ->
        case decompH u of
            Left e -> runHeftiaEffects $ i $ hfmap heftiaEffects e
            Right e -> liftSigT e

reinterpretH ::
    (TransHeftia c h, UnionH u, HFunctor (u (e : es)), HFunctor e, c f) =>
    (e (HeftiaEffects h u (e ': es) f) ~> HeftiaEffects h u (e ': es) f) ->
    HeftiaEffects h u (e ': es) f ~> HeftiaEffects h u (e ': es) f
reinterpretH i a =
    heftiaEffects $ ($ runHeftiaEffects a) $ reelaborateHT \u ->
        case decompH u of
            Left e -> runHeftiaEffects $ i $ hfmap heftiaEffects e
            Right e -> liftSigT $ weakenH e

transformAllH ::
    ( TransHeftia c h
    , UnionH u
    , UnionH u'
    , HFunctor (u es)
    , HFunctor (u' es')
    , c f
    ) =>
    (forall g. u es g ~> u' es' g) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u' es' f
transformAllH f = overHeftiaEffects $ transformHT f
{-# INLINE transformAllH #-}

translate ::
    ( TransHeftia c h
    , UnionH u
    , HFunctor (u (e : es))
    , HFunctor (u (e' : es))
    , HFunctor e
    , HFunctor e'
    , c f
    ) =>
    (e (HeftiaEffects h u (e' ': es) f) ~> e' (HeftiaEffects h u (e' ': es) f)) ->
    HeftiaEffects h u (e ': es) f ~> HeftiaEffects h u (e' ': es) f
translate f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ translateT \u ->
        case decompH u of
            Left e -> inject0H $ hfmap runHeftiaEffects $ f $ hfmap heftiaEffects e
            Right e -> weakenH e

translateAll ::
    ( TransHeftia c h
    , UnionH u
    , UnionH u'
    , HFunctor (u es)
    , HFunctor (u' es')
    , c f
    ) =>
    (u es (HeftiaEffects h u' es' f) ~> u' es' (HeftiaEffects h u' es' f)) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u' es' f
translateAll f =
    overHeftiaEffects $ translateT (hfmap runHeftiaEffects . f . hfmap heftiaEffects)
{-# INLINE translateAll #-}

interposeH ::
    forall e h u es f c.
    (TransHeftia c h, UnionH u, MemberH u e es, HFunctor (u es), c f) =>
    (e (HeftiaEffects h u es f) ~> HeftiaEffects h u es f) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u es f
interposeH f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ reelaborateHT \u ->
        let u' = hfmap (interposeH f . heftiaEffects) u
         in case projectH @_ @e u' of
                Just e -> runHeftiaEffects $ f e
                Nothing -> liftSigT $ hfmap runHeftiaEffects u'

interceptH ::
    forall e h u es f c.
    (TransHeftia c h, UnionH u, MemberH u e es, HFunctor (u es), HFunctor e, c f) =>
    (e (HeftiaEffects h u es f) ~> e (HeftiaEffects h u es f)) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u es f
interceptH f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ translateT \u ->
        let u' = hfmap (interceptH f . heftiaEffects) u
         in case projectH @_ @e u' of
                Just e -> injectH $ hfmap runHeftiaEffects $ f e
                Nothing -> hfmap runHeftiaEffects u'

raiseH ::
    forall e hs h u f c.
    ( TransHeftia c h
    , HFunctor (u hs)
    , HFunctor (u (e ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u hs f ~> HeftiaEffects h u (e ': hs) f
raiseH = transformAllH weakenH
{-# INLINE raiseH #-}

raise2H ::
    forall e1 e2 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u hs)
    , HFunctor (u (e1 ': e2 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u hs f ~> HeftiaEffects h u (e1 ': e2 ': hs) f
raise2H = transformAllH weaken2H
{-# INLINE raise2H #-}

raise3H ::
    forall e1 e2 e3 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u hs)
    , HFunctor (u (e1 ': e2 ': e3 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u hs f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': hs) f
raise3H = transformAllH weaken3H
{-# INLINE raise3H #-}

raise4H ::
    forall e1 e2 e3 e4 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u hs)
    , HFunctor (u (e1 ': e2 ': e3 ': e4 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u hs f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': e4 ': hs) f
raise4H = transformAllH weaken4H
{-# INLINE raise4H #-}

raiseUnderH ::
    forall e1 e2 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': hs))
    , HFunctor (u (e1 ': e2 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': hs) f ~> HeftiaEffects h u (e1 ': e2 ': hs) f
raiseUnderH = transformAllH weakenUnderH
{-# INLINE raiseUnderH #-}

raiseUnder2H ::
    forall e1 e2 e3 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': hs))
    , HFunctor (u (e1 ': e2 ': e3 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': hs) f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': hs) f
raiseUnder2H = transformAllH weakenUnder2H
{-# INLINE raiseUnder2H #-}

raiseUnder3H ::
    forall e1 e2 e3 e4 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': hs))
    , HFunctor (u (e1 ': e2 ': e3 ': e4 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': e3 ': hs) f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': e4 ': hs) f
raiseUnder3H = transformAllH weakenUnder3H
{-# INLINE raiseUnder3H #-}

raise2UnderH ::
    forall e1 e2 e3 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': hs))
    , HFunctor (u (e1 ': e2 ': e3 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': hs) f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': hs) f
raise2UnderH = transformAllH weaken2UnderH
{-# INLINE raise2UnderH #-}

raise2Under2H ::
    forall e1 e2 e3 e4 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': hs))
    , HFunctor (u (e1 ': e2 ': e3 ': e4 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': hs) f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': e4 ': hs) f
raise2Under2H = transformAllH weaken2Under2H
{-# INLINE raise2Under2H #-}

raise3UnderH ::
    forall e1 e2 e3 e4 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': hs))
    , HFunctor (u (e1 ': e2 ': e3 ': e4 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': hs) f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': e4 ': hs) f
raise3UnderH = transformAllH weaken3UnderH
{-# INLINE raise3UnderH #-}

flipHeftia ::
    forall e1 e2 hs h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': hs))
    , HFunctor (u (e2 ': e1 ': hs))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': hs) f ~> HeftiaEffects h u (e2 ': e1 ': hs) f
flipHeftia = transformAllH flipUnionH
{-# INLINE flipHeftia #-}

flipHeftia3 ::
    forall e1 e2 e3 es h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': es))
    , HFunctor (u (e3 : e2 : e1 : es))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': e3 ': es) f ~> HeftiaEffects h u (e3 ': e2 ': e1 ': es) f
flipHeftia3 = transformAllH flipUnion3H
{-# INLINE flipHeftia3 #-}

flipHeftiaUnder ::
    forall e1 e2 e3 es h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': es))
    , HFunctor (u (e1 : e3 : e2 : es))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': e3 ': es) f ~> HeftiaEffects h u (e1 ': e3 ': e2 ': es) f
flipHeftiaUnder = transformAllH flipUnionUnderH
{-# INLINE flipHeftiaUnder #-}

rotate3H ::
    forall e1 e2 e3 es h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': es))
    , HFunctor (u (e2 : e3 : e1 : es))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': e3 ': es) f ~> HeftiaEffects h u (e2 ': e3 ': e1 ': es) f
rotate3H = transformAllH rot3H
{-# INLINE rotate3H #-}

rotate3H' ::
    forall e1 e2 e3 es h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': es))
    , HFunctor (u (e3 : e1 : e2 : es))
    , c f
    , UnionH u
    ) =>
    HeftiaEffects h u (e1 ': e2 ': e3 ': es) f ~> HeftiaEffects h u (e3 ': e1 ': e2 ': es) f
rotate3H' = transformAllH rot3H'
{-# INLINE rotate3H' #-}

bundle2H ::
    forall u' e1 e2 es h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': es))
    , HFunctor (u (u' '[e1, e2] ': es))
    , c f
    , UnionH u
    , UnionH u'
    ) =>
    HeftiaEffects h u (e1 ': e2 ': es) f ~> HeftiaEffects h u (u' '[e1, e2] ': es) f
bundle2H = transformAllH bundleUnion2H
{-# INLINE bundle2H #-}

bundle3H ::
    forall u' e1 e2 e3 es h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': es))
    , HFunctor (u (u' '[e1, e2, e3] : es))
    , c f
    , UnionH u
    , UnionH u'
    ) =>
    HeftiaEffects h u (e1 ': e2 ': e3 ': es) f ~> HeftiaEffects h u (u' '[e1, e2, e3] ': es) f
bundle3H = transformAllH bundleUnion3H
{-# INLINE bundle3H #-}

bundle4H ::
    forall u' e1 e2 e3 e4 es h u f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': e4 ': es))
    , HFunctor (u (u' '[e1, e2, e3, e4] : es))
    , c f
    , UnionH u
    , UnionH u'
    ) =>
    HeftiaEffects h u (e1 ': e2 ': e3 ': e4 ': es) f
        ~> HeftiaEffects h u (u' '[e1, e2, e3, e4] ': es) f
bundle4H = transformAllH bundleUnion4H
{-# INLINE bundle4H #-}

unbundle2H ::
    forall e1 e2 es h u u' f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': es))
    , HFunctor (u (u' '[e1, e2] ': es))
    , c f
    , UnionH u
    , UnionH u'
    ) =>
    HeftiaEffects h u (u' '[e1, e2] ': es) f ~> HeftiaEffects h u (e1 ': e2 ': es) f
unbundle2H = transformAllH unbundleUnion2H
{-# INLINE unbundle2H #-}

unbundle3H ::
    forall e1 e2 e3 es h u u' f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': es))
    , HFunctor (u (u' '[e1, e2, e3] ': es))
    , c f
    , UnionH u
    , UnionH u'
    ) =>
    HeftiaEffects h u (u' '[e1, e2, e3] ': es) f ~> HeftiaEffects h u (e1 ': e2 ': e3 ': es) f
unbundle3H = transformAllH unbundleUnion3H
{-# INLINE unbundle3H #-}

unbundle4H ::
    forall e1 e2 e3 e4 es h u u' f c.
    ( TransHeftia c h
    , HFunctor (u (e1 ': e2 ': e3 ': e4 ': es))
    , HFunctor (u (u' '[e1, e2, e3, e4] ': es))
    , c f
    , UnionH u
    , UnionH u'
    ) =>
    HeftiaEffects h u (u' '[e1, e2, e3, e4] ': es) f
        ~> HeftiaEffects h u (e1 ': e2 ': e3 ': e4 ': es) f
unbundle4H = transformAllH unbundleUnion4H
{-# INLINE unbundle4H #-}

hoistHeftiaEffects ::
    (TransHeftia c h, HFunctor (u es), c f, c g) =>
    (f ~> g) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u es g
hoistHeftiaEffects f = overHeftiaEffects $ hoistHeftia f
{-# INLINE hoistHeftiaEffects #-}

overHeftiaEffects ::
    (h (u es) f a -> h' (u' es') g b) ->
    HeftiaEffects h u es f a ->
    HeftiaEffects h' u' es' g b
overHeftiaEffects f = heftiaEffects . f . runHeftiaEffects
{-# INLINE overHeftiaEffects #-}

hoistInterpose ::
    forall e h u es fr u' es' f c c'.
    ( TransHeftia c h
    , HFunctor (u es)
    , TransFreer c' fr
    , Union u'
    , Member u' e es'
    , c (FreerEffects fr u' es' f)
    , c' f
    ) =>
    (e ~> FreerEffects fr u' es' f) ->
    HeftiaEffects h u es (FreerEffects fr u' es' f)
        ~> HeftiaEffects h u es (FreerEffects fr u' es' f)
hoistInterpose f = hoistHeftiaEffects $ interpose f
{-# INLINE hoistInterpose #-}

interposeIns ::
    forall e h u es fr u' es' f c c'.
    ( TransHeftia c h
    , HFunctor (u es)
    , TransFreer c' fr
    , Union u'
    , Member u' e es'
    , c (FreerEffects fr u' es' f)
    , c' f
    , c' (h (u es) (FreerEffects fr u' es' f))
    ) =>
    (e ~> HeftiaEffects h u es (FreerEffects fr u' es' f)) ->
    HeftiaEffects h u es (FreerEffects fr u' es' f)
        ~> HeftiaEffects h u es (FreerEffects fr u' es' f)
interposeIns f a =
    heftiaEffects
        . ($ runHeftiaEffects a)
        $ interpretLowerH
        $ runFreerEffects
            >>> interpretFT
                (liftLowerHT . freerEffects . liftLowerFT)
                \u -> case project @_ @e u of
                    Just e -> runHeftiaEffects $ f e
                    Nothing -> liftLowerHT $ freerEffects $ liftInsT u

liftLowerH :: (TransHeftia c h, c f, HFunctor (u es)) => f ~> HeftiaEffects h u es f
liftLowerH = heftiaEffects . liftLowerHT

elaborated :: (TransHeftia c h, UnionH u, HFunctor (u '[]), c f) => HeftiaEffects h u '[] f ~> f
elaborated = runElaborateH absurdUnionH . runHeftiaEffects
{-# INLINE elaborated #-}

type Hef es f = HeftiaEffects HeftiaChurchT SumUnionH es f

-- type HefA es f = HeftiaEffects (HeftiaFinalT Applicative) SumUnionH es f

type e <<: es = MemberH SumUnionH e es
