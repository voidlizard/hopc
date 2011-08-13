module Compilers.Hopc.Backend.TinyC.FromClosure where

import Prelude hiding (init)

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.KTree (KId)
import Compilers.Hopc.Frontend.Closure
import Compilers.Hopc.Backend.TinyC.IR

import Data.List
import Data.Maybe

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Generics.Biplate


import Text.PrettyPrint.HughesPJClass (prettyShow)

import Text.Printf
import Debug.Trace

data Conv = Conv { regno :: Int, regmap :: M.Map KId R }

type ConvM = StateT Conv CompileM

retvalReg = (R 1)

convert  :: Closure -> CompileM IR
convert k = trace "TRACE: FromClosure :: convert " $ do
    
    v <- evalStateT (tr retvalReg k) init

    return $ IR v
    
    where 
          tr :: R -> Closure -> ConvM [Instr]

          tr r (CLet n e e1) = do
            a <- trB (n,e)
            b <- tr r e1
            return $ a ++ b

          tr r (CLetR binds e)   = do
            binds' <- mapM trB binds
            e' <- tr r e
            return $ concat binds' ++ e'

          tr r (CApplCls n args) = do
            rs <- mapM getReg' args >>= return . unwords . ((:) n) .  map (maybe "r?" prettyShow)
            return $ [opc (CALL n "") rs] ++ [opc (MOV retvalReg r) "ret. val."]
          
          tr r (CApplDir n args) = do
            rs <- mapM getReg' args >>= return . unwords . ((:) n) .  map (maybe "r?" prettyShow)
            return $ [opc (CALL n "") rs] ++ [opc (MOV retvalReg r) (printf "retval -> %s" (prettyShow r))]

          tr r (CMakeCls n args) = return $ [opc NOP "make-closure"]

          tr r (CVar n)          = do
            r2 <- getReg n
            return $ [opc (MOV r2 r) (printf "%s -> %s" n (prettyShow r))]

          tr r (CCond n e1 e2) = do
            c1 <- tr r e1
            c2 <- tr r e2
            r  <- getReg n

            let l1 = "L1" -- FIXME: generate label
            let ll1 = LABEL l1
            let l3 = "L3"
            let ll3 = LABEL l3

            return $ op (CJUMP (JumpFake r) l1) : c2 ++ op (JUMP "L3") : (op ll1) : c1 ++ op (LABEL "L3") : []

          tr r x = return $ [opc NOP "unsupported"]

          trB :: (KId, Closure) -> ConvM [Instr]

          trB (n, (CInt v)) = do
            r <- addReg n 
            return $ [opc (CONST (show v) r) (printf "%d -> %s" v n)]

          trB (n, (CStr s)) = do
            r <- addReg n
            return $ [opc (CONST "Sx" r) (printf "'%s' -> %s" s n)]

          trB (n, (CVar k)) = do
            r1 <- addReg n
            r2 <- getReg k
            return $ [opc (MOV r2 r1) (printf "%s -> %s" k n )]

          trB (n, e) = addReg n >>= flip tr e

          addReg :: KId -> ConvM R
          addReg n = do
            reg <- newreg
            modify (\x@(Conv {regmap=r}) -> x{regmap=M.insert n reg r})
            return reg

          getReg n = do
            r <- gets (M.lookup n . regmap)
            if isJust r then (return.fromJust) r else addReg n

          getReg' n = do
            gets (M.lookup n . regmap) >>= return

          newreg :: ConvM R
          newreg = do
            s@(Conv {regno = n}) <- get
            let r = R (n+1)
            put s{regno = n+1}
            return r

          init  = Conv { regno = 3, regmap = M.empty } -- FIXME: remove the hardcode

