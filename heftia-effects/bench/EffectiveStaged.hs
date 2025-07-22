{-# LANGUAGE BlockArguments, TemplateHaskell, ImpredicativeTypes, PartialTypeSignatures, LambdaCase, TypeFamilies #-}
module EffectiveStaged where

import "effective" Control.Effect
import "effective" Control.Effect
import "effective" Control.Effect.CodeGen
import "effective" Control.Effect.CodeGen.Nondet
import "effective" Control.Effect.Yield
import "effective" Control.Effect.State.Strict
import "effective" Control.Effect.Reader
import "effective" Control.Effect.Except 
import "effective" Control.Effect.Alternative
import "effective" Control.Monad.Trans.Push
import "effective" Control.Monad.Trans.List
import "effective" Control.Effect.Yield
import "effective" Control.Effect.Internal.AlgTrans (weakenC)

catchGen :: forall sig m. Members '[CodeGen, UpOp m, Catch (Up ()), Throw (Up ())] sig 
         => Up Int -> Up (Int -> m ()) -> Prog sig (Up ())
catchGen cN self = 
  do b <- split [|| $$cN > 0 ||]
     if b 
      then catch (up [|| $$self ($$cN - 1)||]) (\(_ :: Up ()) -> throw @(Up ()) [||()||])
      else throw @(Up ()) [|| () ||]

countdownGen :: Members '[CodeGen, UpOp m, Put (Up Int), Get (Up Int)] sig 
             => Up (m ()) -> Prog sig (Up ())
countdownGen self = 
  do cs <- get @(Up Int)
     b <- split [|| $$cs > 0 ||]
     if b then do put [|| $$cs - 1 ||]; up self
          else return [|| () ||]

localGen :: forall sig m. Members '[CodeGen, UpOp m, Ask (Up Int), Local (Up Int)] sig
         => Up Int -> Up (Int -> m Int) -> Prog sig (Up Int)
localGen cN self =
  do b <- split [|| $$cN > 0 ||]
     if b
       then local @(Up Int) (\r -> [|| $$r + 1 ||]) (up [|| $$self ($$cN - 1) ||])
       else ask @(Up Int)

pythGen :: forall sig m. Members '[CodeGen, Choose, Empty, UpOp m] sig
        => Up Int -> Up (Int -> m Int) -> Prog sig (Up (Int, Int, Int))
pythGen cN cChoose = 
  do x <- up ([|| $$cChoose $$cN||])
     y <- up ([|| $$cChoose $$cN||])
     z <- up ([|| $$cChoose $$cN||])
     genIf [|| $$x * $$x + $$y * $$y == $$z * $$z ||] 
       (return [|| ($$x, $$y, $$z) ||])
       empty

chooseGen :: forall sig m. Members '[CodeGen, Choose, Empty, UpOp m] sig
        => Up Int -> Up (Int -> m Int) -> Prog sig (Up Int)
chooseGen cN self =
  genIf [|| $$cN > 0 ||]
    (up [|| $$self ($$cN - 1) ||] <|> return cN)
    empty 
    
coroutine1Gen :: forall sig m. Members '[CodeGen, Yield (Up Int) (Up Int), UpOp m] sig
              => Up [Int] -> Up ([Int] -> m [Int]) -> Prog sig (Up [Int])
coroutine1Gen cXs self =
  do genCase cXs \case
       Nothing         -> return [|| [] ||]
       Just (cX, cXs') -> 
         do cY <- yield @(Up Int) @(Up Int) cX
            rs <- up [|| $$self $$cXs' ||]
            return [|| $$cY : $$rs ||]

coroutine2Gen :: forall sig m a. Members '[CodeGen, Yield (Up Int) (Up Int), UpOp m] sig
              => Up Int -> Up (Int -> m a) -> Prog sig (Up a)
coroutine2Gen cA self = 
  do cB <- yield [|| $$cA + 100 ||]
     up [|| $$self $$cB ||]
     
type R1 m = ReaderT () m
type R2 m = ReaderT () (R1 m)
type R3 m = ReaderT () (R2 m)
type R4 m = ReaderT () (R3 m)
type R5 m = ReaderT () (R4 m)
type M = R5 (ListT (R5 Identity))

rAT :: AlgTrans '[Ask (Up ())]  '[] '[ReaderT (Up ())] Monad
rAT = readerAskAT @(Up ())

upR5 :: AlgTrans '[UpOp (R5 Identity)] '[Ask (Up ()), CodeGen, UpOp Identity] '[] Monad
upR5 = weakenC $
         upReader @() @(R4 Identity) `pipeAT`
         upReader @() @(R3 Identity) `pipeAT`
         upReader @() @(R2 Identity) `pipeAT`
         upReader @() @(R1 Identity) `pipeAT`
         upReader @() @(Identity) 