{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable  #-}
module Compilers.Hopc.Backend.TinyC.CPrinter where

import Compilers.Hopc.Backend.TinyC.VM
import Compilers.Hopc.Compile

import Data.List
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans

import Text.Printf

data PrintC = PrintC { includes :: [String] }

emptyPrintC = PrintC []

printC :: PrintC -> VM -> CompileM String
printC x (VM instr) = do

    let ls = M.fromList $ zipWith (,) (foldr wl [] instr) [0..]
    let numOf l = M.lookup l ls

    let fls = map line instr

    return $ intercalate "\n"  fls

    where wl (I (LABEL n) _) acc = n:acc
          wl _  acc = acc

          line (I (LABEL n) _) = label n 
          line (I (RET)_ )     = ret
          line (I (JUMP n)_ )  = jmp n
          line (I (MOV r1 r2) _ ) = mov r1 r2
          line (I (CALL_FOREIGN n rs _) _ ) = callForeign n rs
          line (I (CALL_LOCAL n rs _) _ ) = callLocal n rs
          line (I (CONST l r) _ ) = const l r

          line (I (CJUMP (JumpFake r) l0 l) _ ) = indent ++ printf "IFTRUE(%s, %s);" (reg r) l

          line _               = indent ++ "UNSUPPORTED;"

          label :: LabelId -> String

          label l   = newline ++ printf "%s case %3d:" (printf "%-4s" (l++":") :: String) (0::Int)
          ret       = indent  ++ printf "RET;"
          jmp l     = indent  ++ printf "JMP(%s);" l

          mov r1 r2 = indent ++ printf "MOV(%s, %s);" (reg r1) (reg r2)

          callForeign :: LabelId -> [R] -> String
          callForeign l rs = indent ++ printf "CALL_FOREIGN(%s);" (commasep (l:regs rs))
          
          callLocal :: LabelId -> [R] -> String
          callLocal l rs   = indent ++ printf "CALL_LOCAL(%s);" (commasep (l:regs rs))

          const s r = indent ++ printf "CONST(%s, %s);" s (reg r)

          reg :: R -> String
          reg (R n) = printf "r%d" n

          regs :: [R] -> [String]
          regs = map reg

          commasep :: [String] -> String
          commasep ls = intercalate ", " ls

          indent  = replicate 4 ' '
          newline = "\n"


