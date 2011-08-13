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
    
    v <- evalStateT (tr k) init

    return $ IR v
    
    where 
          tr :: Closure -> ConvM [Instr]

          tr (CLet n e e1) =  liftM2 (++) (trB (n,e)) (tr e1)   -- lift (++) (trB (n, e)) (tr e1)

          tr (CLetR binds e)   = do
            binds' <- mapM trB binds
            e' <- tr e
            return $ concat binds' ++ e'

          tr (CApplCls n args) = return $ [op $ CALL n ""]
          tr (CApplDir n args) = return $ [op $ CALL n ""]
          tr (CMakeCls n args) = return $ [opc NOP "make-closure"]
          tr (CVar n)          = error "CVAR WTF?" -- [return $ opc (MOV (R 111) (R 222)) (printf "%s -> R0" n)] -- MOV Rn Rb

          tr (CCond n e1 e2) = do
            c1 <- tr e1
            c2 <- tr e2

            let l1 = "L1" -- FIXME: generate label
            let ll1 = LABEL l1
            let l3 = "L3"
            let ll3 = LABEL l3

            return $ op (CJUMP (JumpFake (R 0)) l1) : c2 ++ op (JUMP "L3") : (op ll1) : c1 ++ op (LABEL "L3") : []

          tr x = return $ [opc NOP "unsupported"]

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

--          trB (n, e)        = return $ tr e ++ [opc (MOV (R 1) (R 0)) (printf "expr -> %s" n) ]

          trB (n, e@(CApplCls _ _)) = do
            r2 <- addReg n
            let rr = retvalReg
            e' <- tr e
            return $ e' ++ [op (MOV rr r2)]


          trB (n, e@(CApplDir _ _)) = do
            r2 <- addReg n
            let rr = retvalReg
            e' <- tr e
            return $ e' ++ [op (MOV rr r2)]

          trB (n, e)  = do
            r <- getReg n
            trace ("TRACE: died at " ++ (prettyShow r) ++ " " ++ prettyShow e) $ return ()
            undefined
          -- error "JOPA --- what reg binded to the expression? " ++ n -- (prettyShow e) -- return $ tr e ++ [opc (MOV (R 1) (R 0)) (printf "expr -> %s" n) ]

          addReg :: KId -> ConvM R
          addReg n = do
            reg <- newreg
            modify (\x@(Conv {regmap=r}) -> x{regmap=M.insert n reg r})
            return reg

          getReg n = do
            r <- gets (M.lookup n . regmap)
            if isJust r then (return.fromJust) r else addReg n

          newreg :: ConvM R
          newreg = do
            s@(Conv {regno = n}) <- get
            let r = R (n+1)
            put s{regno = n+1}
            return r

          init  = Conv { regno = 3, regmap = M.empty } -- FIXME: remove the hardcode

