module Compilers.Hopc.Backend.TinyC.VM where

import Data.List

import Compilers.Hopc.Id (KId)
import Compilers.Hopc.Backend.TinyC.Lit
import qualified Compilers.Hopc.Backend.TinyC.R as R

import Compiler.Hoopl

data R =  R0 | R1  | R2  | R3  | R4  | R5
             | R6  | R7  | R8  | R9  | R10
             | R11 | R12 | R13 | R14 | R15
             deriving (Eq, Ord, Enum, Show)

instance R.R R where
    avail   = [R1 .. R15]
    allRegs = [R0 .. R15]

closureReg = R0

type Slot = Int

data RT = RVoid | RReg R

data Op  =   Label Label
           | CallC Label R [R] RT
           | CallF Label KId [R] RT
           | CallL Label KId [R] RT
           | MkClos KId [R] R
           | Const Lit R
           | Move  R R
           | Spill   R Slot
           | Unspill Slot R
           | Branch Label 
           | BranchFalse R Label
           | BranchTrue  R Label
           | Nop
           | Return

data Proc = Proc { name :: KId
                 , arity :: Int
                 , slotnum :: Int
                 , body :: [Op]
                 }
            deriving (Show)

instance Show (Op) where
  show (Label lbl)      = show lbl ++ ":"
  show (CallC l r rs rt)     = ind $ "call-closure " ++ show l ++ " " ++ (show r) ++ " " ++ (intercalate ", " (map show rs)) ++ " " ++ show rt
  show (CallL l n rs rt)     = ind $ "call-local " ++ show l ++ " " ++ n  ++ " " ++ (intercalate ", " (map show rs)) ++ " " ++ show rt
  show (CallF l n rs rt)     = ind $ "call-foreign " ++ show l ++ " " ++ n ++ " " ++ (intercalate ", " (map show rs)) ++ " " ++ show rt
  show (MkClos x vs v)    = ind $ "make-closure " ++ x ++ " " ++ (intercalate ", " (map show vs)) ++ " -> " ++  (show v)
  show (Const (LInt n) r) = ind $ "iconst " ++ (show n) ++ " " ++ (show r)
  show (Const (LStr s) r) = ind $ "sconst " ++ (show s) ++ " " ++ (show r)
  show (Move a b)         = ind $ "move " ++ (show a) ++ " " ++ (show b)
  show (Spill r n)        = ind $ "spill " ++ (show r) ++ " " ++ (show n)
  show (Unspill n r)      = ind $ "unspill " ++ (show n) ++ " " ++ (show r)
  show (Branch l)         = ind $ "branch " ++  " " ++ (show l)
  show (BranchTrue r l1)  = ind $ "branch-true "   ++ (show r) ++ " " ++ (show l1)
  show (BranchFalse r l1) = ind $ "branch-false " ++ (show r) ++ " " ++ (show l1)
  show (Nop)              = ind $ "nop" 
  show (Return)           = ind $ "return "

ind :: String -> String
ind x = "    " ++ x

instance Show (RT) where
    show (RVoid)  = "; void"
    show (RReg r) = "; -> " ++ show r


