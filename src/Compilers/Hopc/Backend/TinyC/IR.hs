{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.IR where

import Compiler.Hoopl
import Data.List (intercalate)
import Control.Monad.Trans
import Compilers.Hopc.Compile
import Compilers.Hopc.Id (KId)
import Compilers.Hopc.Backend.TinyC.Lit

type M = CheckingFuelMonad (SimpleUniqueMonad) 

data CallT = Direct KId Bool  | Closure KId Bool

data Insn e x where
    Label   :: Label ->                     Insn C O
    Call    :: Label -> CallT -> [KId] -> KId -> Insn O C 
    MkClos  :: KId   -> [KId] -> KId ->     Insn O O
    Const   :: Lit   -> KId   ->            Insn O O
    Assign  :: KId   -> KId   ->            Insn O O
    Branch  :: Label          ->            Insn O C
    Cond    :: KId   -> Label -> Label ->   Insn O C
    Return  :: KId            ->            Insn O C

instance NonLocal Insn where
    entryLabel (Label l)      = l
    successors (Branch l)     = [l]
    successors (Cond _ t f)   = [t, f]
    successors (Return _)     = []
    successors (Call l _ _ _)   = [l]
--    successors (MkClos _ _ _) = []
--    successors (Const _ _)    = []
--    successors (Assign _ _)   = []


data Proc = Proc { name :: KId, args :: [KId], entry :: Label, body :: Graph Insn C C }

runM :: M a -> a
runM m = runSimpleUniqueMonad $ runWithFuel 0 m

instance Show (Insn e x) where
  show (Label lbl)      = show lbl ++ ":"
  show (Call _ (Direct  n False) a t) = ind $ "call-direct " ++ n ++ " " ++ (intercalate ", " a)  ++ " -> " ++ t
  show (Call _ (Closure n False) a t) = ind $ "call-closure " ++ n ++ " " ++  (intercalate ", " a)  ++ " -> " ++ t
  show (Call _ (Direct  n True) a t) = ind $ "call-direct-tail " ++ n ++ " " ++ (intercalate ", " a)  ++ " -> " ++ t
  show (Call _ (Closure n True) a t) = ind $ "call-closure-tail " ++ n ++ " " ++  (intercalate ", " a)  ++ " -> " ++ t
  show (MkClos x vs v) = ind $ "make-closure " ++ x ++ " " ++ (intercalate ", " vs) ++ " -> " ++  v
  show (Const (LInt n) v) = ind $ "iconst " ++ (show n) ++ " " ++ v
  show (Const (LStr s) v) = ind $ "sconst " ++ (show s) ++ " " ++ v 
  show (Assign a b)       = ind $ "assign " ++ a ++ " " ++ b
  show (Branch l)         = ind $ "branch " ++ (show l)
  show (Cond v l1 l2)     = ind $ "cond-branch " ++ v ++ " " ++ (show l1) ++ " " ++ (show l2)
  show (Return v)          = ind $ "return " ++ v 

ind :: String -> String
ind x = "    " ++ x
