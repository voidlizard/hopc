{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Compilers.Hopc.Backend.TinyC.Live where

import Compilers.Hopc.Id
import Compilers.Hopc.Compile

import Compilers.Hopc.Backend.TinyC.IR

import Compiler.Hoopl hiding (Block)

import Data.Maybe
import qualified Data.Set as S

type Live = S.Set KId 
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
    live (Assign n n1)    f = addUses f [n1]
    live (Const _ n)      f = addUses f [n]
    live (Branch l)       f = fact f l
    live (Cond n tl fl)   f = addUses (fact f tl `S.union` fact f fl) [n]
    live (Call l (Closure n _) an rn) f = addUses (fact f l) (n:an)   -- addUses (fact f l `S.un` (n:an)  -- addUses (S.delete rn $ fact f l) (n:an)
    live (Call l (Direct n _) an rn) f  = addUses (fact f l) an -- addUses (S.delete rn $ fact f l) an
    live (MkClos n an rn) f = addUses f (n:an)
    live (Return n)       _ = addUses (fact_bot liveLattice) [n,activationRecordVariable]

    fact :: FactBase (S.Set KId) -> Label -> Live
    fact f l = fromMaybe S.empty $ lookupFact l f
    
    addUses :: Live -> [KId] -> Live
    addUses s rs = s `S.union` S.fromList rs
    delUse :: Live -> KId -> Live
    delUse s n = S.delete n s

deadAsstElim :: forall m . FuelMonad m => BwdRewrite m Insn Live
deadAsstElim = mkBRewrite d
  where
    d :: Insn e x -> Fact x Live -> m (Maybe (Graph Insn e x))
    d (Assign n x) live | not (n `S.member` live) = return $ Just emptyGraph
    d (Const _ n) live  | not (n `S.member` live) = return $ Just emptyGraph
    d _ _ = return Nothing

live entry g = do
   let bwd  = BwdPass { bp_lattice = liveLattice, bp_transfer = liveness
                       ,bp_rewrite = noBwdRewrite  }

   (_, live, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) g noFacts
   return live

