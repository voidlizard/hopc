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

data Conv = Conv { regno :: Int, lbl :: Int, regmap :: M.Map KId R, funlbl :: M.Map KId LabelId }

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
--            l <- getFunLbl n
            r <- getReg n  -- TODO: CALL-CLOSURE
            let r' = prettyShow r
            return $ [opc (CALL r' "") ("call-closure")] ++ [opc (MOV retvalReg r) "ret. val."]
          
          tr r (CApplDir n args) = do
            rs <- mapM getReg' args >>= return . unwords . ((:) n) .  map (maybe "r?" prettyShow)
            l <- getFunLbl n
            return $ [opc (CALL l "") rs] ++ [opc (MOV retvalReg r) (printf "retval -> %s" (prettyShow r))]

          tr r (CMakeCls n args) = do
            rr <- newreg
            return $ [opc NOP "make-closure", opc (MOV rr r) n]

          tr r (CVar n) = do
            r2 <- getReg n
            return $ [opc (MOV r2 r) (printf "%s -> %s" n (prettyShow r))]

          tr r (CCond n e1 e2) = do
            c1 <- tr r e1
            c2 <- tr r e2
            r  <- getReg n

            l1 <- newlbl 
            l3 <- newlbl

            let ll1 = LABEL l1
            let ll3 = LABEL l3

            return $ op (CJUMP (JumpFake r) l1) : c2 ++ op (JUMP l3) : (op ll1) : c1 ++ op ll3 : []

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

          trB (n, e@(CFun (Fun fn args free k))) = do
 
            fstart <- newlbl
            addFunLbl  n fstart 

            st@(Conv{lbl=l, funlbl=fl}) <- get

            let newst = init{lbl = l, funlbl = fl}

            (code, (Conv{lbl=nl})) <- lift $ flip runStateT newst $ do
                mapM_ addReg (args ++ free)
                tr retvalReg k
 
            modify (\x -> x{lbl=nl})

            return $ opc (LABEL fstart) fn : code ++ [opc RET "ret"]


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

          newlbl :: ConvM LabelId
          newlbl = do
            s@(Conv {lbl = n}) <- get
            put s{lbl = n+1}
            return $ "L" ++ (show n)

          addFunLbl :: KId -> LabelId -> ConvM ()
          addFunLbl n l = do
            modify (\x@(Conv {funlbl=fl}) -> x{funlbl=M.insert n l fl})

          getFunLbl :: KId -> ConvM LabelId -- TODO: error handling (unknown label)
          getFunLbl n = do
            l <- gets (M.lookup n . funlbl)
            if isJust l
                then (return.fromJust) l
                else do
                    e <- lift $ getEntry n
                    if isJust e
                        then return n
                        else error $ "UNKNOWN LABEL FOR: " ++ n  -- TODO: error handling (unknown label)

          init  = Conv {regno = 3, lbl = 0, regmap = M.empty, funlbl = M.empty} -- FIXME: remove the hardcode

