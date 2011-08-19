{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.IR where

import Compilers.Hopc.Id (KId)
import Compilers.Hopc.Compile
import Prelude hiding (succ)
import Compiler.Hoopl
import Control.Monad
import Control.Monad.Trans

import Compilers.Hopc.Backend.TinyC.VM
import Text.PrettyPrint.HughesPJClass

type M = (CheckingFuelMonad (SimpleUniqueMonad))

data Insn e x where
  Label  :: Label   ->                          Insn C O
  Const  :: R               ->                  Insn O O
  Store  :: R       -> R    ->                  Insn O O
  MClos  :: String  -> [R] ->                   Insn O O
  Branch :: Label   ->                          Insn O C
  Cond   :: R       -> Label  -> Label  ->      Insn O C
  CallL  :: LabelId -> [R] -> Label   ->        Insn O C
  CallC  :: R       -> [R] -> Label   ->        Insn O C
  CallF  :: KId     -> [R] -> Label   ->        Insn O C 
  Return ::                                     Insn O C

data Proc = Proc { name :: LabelId, entry :: Label, body :: Graph Insn C C }

instance NonLocal Insn where
  entryLabel (Label l)      = l
  successors (Branch l)     = [l]
  successors (Cond _ t f)   = [t, f]
  successors (CallL  _ _ l) = [l]
  successors (CallC  _ _ l) = [l]
  successors (CallF  _ _ l) = [l]
  successors (Return)       = []

instance Show (Insn e x) where
  show (Label lbl)     = show lbl ++ ":"
  show (Const r)       = ind $ "const C " ++ (prettyShow r)
  show (Store r1 r2)   = ind $ "mov " ++ (prettyShow r1) ++ " " ++ (prettyShow r2)
  show (MClos s rs)    = ind $ "makeclosure " ++ s ++ " " ++ unwords (map prettyShow rs)
  show (Branch l)      = ind $ "jmp " ++ show l
  show (Cond r l1 l2)  = ind $ "jmp-cond " ++ (prettyShow r) ++ " " ++ (show l1) ++ " " ++ (show l2)
  show (CallL s rs l)  = ind $ "call-local " ++ s ++ " " ++ unwords (map prettyShow rs) ++ " " ++ (show l)
  show (CallF s rs l)  = ind $ "call-foreign " ++ s ++ " " ++ unwords (map prettyShow rs) ++ " " ++ (show l)
  show (CallC r rs l)  = ind $ "call-local " ++ (prettyShow r) ++ " " ++ unwords (map prettyShow rs) ++ " " ++ (show l)
  show (Return)        = ind $ "ret"

--  show (Branch lbl)       = ind $ "goto " ++ show lbl
--  show (Cond e t f)       =
--    ind $ "if " ++ show e ++ " then goto " ++ show t ++ " else goto " ++ show f
--  show (Call ress f cargs succ) =
--    ind $ tuple ress ++ " = " ++ f ++ tuple (map show cargs) ++ " goto " ++ show succ
--  show (Return      rargs) = ind $ "ret " ++ tuple (map show rargs)

ind :: String -> String
ind x = "    " ++ x
