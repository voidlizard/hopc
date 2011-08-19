{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.Live where

import Compilers.Hopc.Id
import Compilers.Hopc.Compile
import Compilers.Hopc.Backend.TinyC.VM (R, LabelId)
import Compilers.Hopc.Backend.TinyC.FromClosure (retvalReg)

import Compilers.Hopc.Backend.TinyC.IR
import qualified Compilers.Hopc.Backend.TinyC.IR

import Compiler.Hoopl hiding (Block)

import Data.Maybe
import qualified Data.Set as S

type Live = S.Set R 
liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice
  { fact_name = "Live variables"
  , fact_bot  = S.empty
  , fact_join = add
  }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = changeIf (S.size j > S.size old)

liveness :: BwdTransfer Insn Live
liveness = mkBTransfer live
  where
    live :: Insn e x -> Fact x Live -> Live
    live (Label _)        f = f
    live (Store _ r1)     f = addUses f [r1]
    live (Const  r1)      f = addUses f [r1]
    live (Branch l)       f = fact f l
    live (Cond r tl fl)   f = addUses (fact f tl `S.union` fact f fl) [r]
    live (CallL _ rs l)   f = addUses (S.delete retvalReg $ fact f l) rs
    live (CallF _ rs l)   f = addUses (S.delete retvalReg $ fact f l) rs
    live (CallC r rs l)   f = addUses (S.delete retvalReg $ fact f l) (r:rs)
    live (MClos _ rs)     f = addUses f rs
    live (Return)         _ = fact_bot liveLattice

    fact :: FactBase (S.Set R) -> Label -> Live
    fact f l = fromMaybe S.empty $ lookupFact l f
    
    addUses :: Live -> [R] -> Live
    addUses s rs = s `S.union` S.fromList rs

--deadAsstElim :: forall m . FuelMonad m => BwdRewrite m Insn Live
--deadAsstElim = mkBRewrite d
--  where
--    d :: Insn e x -> Fact x Live -> m (Maybe (Graph Insn e x))
--    d (Assign x _) live
--        | not (x `S.member` live) = return $ Just emptyGraph
--    d _ _ = return Nothing

