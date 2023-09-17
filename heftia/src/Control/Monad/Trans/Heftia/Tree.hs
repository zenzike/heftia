-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A tree-structured encoded Heftia transformer.
-}
module Control.Monad.Trans.Heftia.Tree where

import Data.Functor.Coyoneda (Coyoneda)

newtype HCoyoneda h f a = HCoyoneda {unHCoyoneda :: Coyoneda (h f) a}

-- todo
