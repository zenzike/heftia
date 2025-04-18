{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [non-determinism]("Data.Effect.NonDet") effects.
-}
module Control.Monad.Hefty.NonDet (
    module Control.Monad.Hefty.NonDet,
    module Data.Effect.NonDet,
)
where

import Control.Applicative (Alternative ((<|>)), (<|>))
#if ( __GLASGOW_HASKELL__ < 906 )
import Control.Applicative (liftA2)
#endif
import Control.Applicative qualified as A
import Control.Arrow ((>>>))
import Control.Monad.Hefty (
    Eff,
    bundleN,
    interpret,
    interpretBy,
    interpretH,
    nil,
    (!+),
    (&),
    type (<<|),
    type (<|),
    type (~>),
 )
import Data.Bool (bool)
import Data.Effect.NonDet
import Data.Effect.Unlift (UnliftIO)
import UnliftIO (Exception, SomeException, throwIO, try)

-- | [NonDet]("Data.Effect.NonDet") effects handler for alternative answer type.
runNonDet
    :: forall f ef a
     . (Alternative f)
    => Eff '[] (Choose ': Empty ': ef) a
    -> Eff '[] ef (f a)
runNonDet =
    bundleN @2
        >>> interpretBy
            (pure . pure)
            ( (\Choose k -> liftA2 (<|>) (k False) (k True))
                !+ (\Empty _ -> pure A.empty)
                !+ nil
            )

-- | [NonDet]("Data.Effect.NonDet") effects handler for monoidal answer type.
runNonDetMonoid
    :: forall ans ef a
     . (Monoid ans)
    => (a -> Eff '[] ef ans)
    -> Eff '[] (Choose ': Empty ': ef) a
    -> Eff '[] ef ans
runNonDetMonoid f =
    bundleN @2
        >>> interpretBy
            f
            ( (\Choose k -> liftA2 (<>) (k False) (k True))
                !+ (\Empty _ -> pure mempty)
                !+ nil
            )

-- | t'Choose' effect handler for alternative answer type.
runChoose
    :: forall f ef a
     . (Alternative f)
    => Eff '[] (Choose ': ef) a
    -> Eff '[] ef (f a)
runChoose =
    interpretBy (pure . pure) \Choose k ->
        liftA2 (<|>) (k False) (k True)

-- | t'Choose' effect handler for monoidal answer type.
runChooseMonoid
    :: forall ans ef a
     . (Semigroup ans)
    => (a -> Eff '[] ef ans)
    -> Eff '[] (Choose ': ef) a
    -> Eff '[] ef ans
runChooseMonoid f =
    interpretBy f \Choose k ->
        liftA2 (<>) (k False) (k True)

-- | t'Empty' effect handler.
runEmpty :: forall a ef. Eff '[] (Empty ': ef) a -> Eff '[] ef (Maybe a)
runEmpty =
    interpretBy
        (pure . Just)
        \Empty _ -> pure Nothing

{- | t'ChooseH' effect elaborator.

    Convert a higher-order effect of the form

        @chooseH :: m a -> m a -> m a@

    into a first-order effect of the form:

        @choose :: m Bool@
-}
runChooseH
    :: (Choose <| ef)
    => Eff (ChooseH ': eh) ef ~> Eff eh ef
runChooseH = interpretH \(ChooseH a b) -> branch a b

-- | Faster than `<|>`.
branch :: (Choose <| ef) => Eff eh ef a -> Eff eh ef a -> Eff eh ef a
branch a b = do
    world <- choose
    bool a b world
{-# INLINE branch #-}

infixl 3 `branch`

-- | Selects one element from the list nondeterministically, branching the control as many times as the number of elements.
choice :: (Choose <| ef, Empty <| ef) => [a] -> Eff eh ef a
choice = \case
    [] -> empty
    x : xs -> pure x `branch` choice xs

-- | Selects one element from the list nondeterministically, branching the control as many times as the number of elements. Uses t'ChooseH'.
choiceH :: (ChooseH <<| eh, Empty <| ef) => [a] -> Eff eh ef a
choiceH = \case
    [] -> empty
    x : xs -> pure x <|> choiceH xs

{- |
Interprets the [NonDet]("Data.Effect.NonDet") effects using IO-level exceptions.

When 'empty' occurs, an 'EmptyException' is thrown, and unless all branches from
 'chooseH' fail due to IO-level exceptions, only the leftmost result is returned
 as the final result.
-}
runNonDetIO
    :: (UnliftIO <<| eh, IO <| ef)
    => Eff (ChooseH ': eh) (Empty ': ef) a
    -> Eff eh ef (Either SomeException a)
runNonDetIO m = try do
    m
        & interpretH
            ( \(ChooseH a b) ->
                try a >>= \case
                    Right x -> pure x
                    Left (_ :: SomeException) -> b
            )
        & interpret (\Empty -> throwIO EmptyException)

-- | Exception thrown when 'empty' occurs in 'runNonDetIO'.
data EmptyException = EmptyException
    deriving stock (Show)
    deriving anyclass (Exception)
