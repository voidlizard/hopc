module Compilers.Hopc.Backend.TinyC.FromClosure where

import Prelude hiding (init)

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.Types
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

            regs <- getRegList args 

            -- TODO: regs <> args -> unknown var check
            -- TODO: regs <> at   -> bad function call

            rc' <- getReg' n  -- TODO: CALL-CLOSURE
            trace ("TRACE: CALL CLOSURE " ++ n ++ " " ++ (show rc')) $ return ()

            when ((not.isJust) rc') $ error $ "UNKNOWN VARIABLE " ++ n

            let rc = fromJust rc'

            return $ [opc (CALL_CLOSURE rc regs) ("call-closure " ++ n)] ++ mov retvalReg r "ret. val."
          
          tr r (CApplDir n args) = do
            entry <- lift $ getEntry n
            applDir r n args entry

          tr r (CMakeCls n args) = do
            let a = maybe [] id args
            regs <- getRegList a 
            rr <- newreg
            return $ [opc NOP ("make-closure " ++ show regs), opc (MOV rr r) n]

          tr r (CVar n) = do
            r2 <- getReg n
--            trace ("TRACE: WTF? " ++ n ++ " " ++ (show r2)) $ return () --- FIXME: CHECK FOR CLOSURE?
--            error "I'm so sorry, but u need typing here"
            return $ mov r2 r (printf "%s -> %s" n (prettyShow r))

          tr r (CCond n e1 e2) = do
            c1 <- tr r e1
            c2 <- tr r e2
            rc  <- getReg n

            l1 <- newlbl
            l3 <- newlbl

            let ll1 = LABEL l1
            let ll3 = LABEL l3

            return $ op (CJUMP (JumpFake rc) l1) : c2 ++ op (JUMP l3) : (op ll1) : c1 ++ op ll3 : []

          tr r (CInt v)= do
              return $ [op (CONST (show v) r)]

          tr r (CStr s) = do
            return $ [op (CONST "Sx" r)]

          tr r1 (CVar k) = do
            r2' <- getReg' k

            when ((not.isJust) r2') $ error $ "UNKNOWN VARIABLE " ++ k

            let r2 = fromJust r2'

            return $ mov r2 r1 ""

          tr r x = return $ [opc NOP $"unsupported " ++ (prettyShow x)]

          trB :: (KId, Closure) -> ConvM [Instr]

          trB (n, e@(CFun (Fun fn args free k))) = do

            fstart <- getFunLbl n

            trace ("TRACE: convert fstart " ++ n ++ " " ++ (show fstart)) $ return ()

            st@(Conv{lbl=l, funlbl=fl}) <- get

            let newst = init{lbl = l, funlbl = fl}

            (code, (Conv{lbl=nl, funlbl=nfl})) <- lift $ flip runStateT newst $ do
                mapM_ addReg (args ++ free)
                tr retvalReg k
 
            modify (\x -> x{lbl=nl, funlbl=nfl})

            return $ opc (LABEL fstart) fn : code ++ [opc RET "ret"]


          trB (n, e) = addReg n >>= flip tr e

          applDir :: R -> KId -> [KId] -> Maybe Entry -> ConvM [Instr]

          applDir r n args (Just (Entry (TFun (TFunForeign ffn) at rt) tp)) = do
            regs <- getRegList args 

            -- TODO: regs <> args -> unknown var check
            -- TODO: regs <> at   -> bad function call

            return $ [opc (CALL_FOREIGN ffn regs) ""] ++ mov retvalReg r (printf "retval -> %s" (prettyShow r)) -- TODO: remove boilerplat

          applDir r n args (Just (Entry (TFun (TFunLocal) at rt) tp)) = do
            regs <- getRegList args
            l <- getFunLbl n

            trace ("TRACE: appDir  " ++ (show n) ++ " " ++ (show l)) $ return ()

            -- TODO: regs <> args -> unknown var check
            -- TODO: regs <> at   -> bad function call

            return $ [opc (CALL_LOCAL l regs) n] ++  mov retvalReg r (printf "retval -> %s" (prettyShow r)) -- TODO: remove boilerplate

          applDir r n args (Just x) = do
            error $ "CALLING NOT-APPLICABLE ENTITY" ++ n --TODO: error handling

          applDir r n args Nothing = do
            trace ("TRACE: appDir  NOT FOUND" ++ (show n)) $ return ()
            error "oops"

          addReg :: KId -> ConvM R
          addReg n = do
            reg <- newreg
            modify (\x@(Conv {regmap=r}) -> x{regmap=M.insert n reg r})
            return reg

          getRegList args = do 
            regs <- mapM getReg' args
            return $ catMaybes regs

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

          addFunLbl :: KId -> LabelId -> ConvM LabelId 
          addFunLbl n l = do
            modify (\x@(Conv {funlbl=fl}) -> x{funlbl=M.insert n l fl})
            return l 

          getFunLbl :: KId -> ConvM LabelId -- TODO: error handling (unknown label)
          getFunLbl n = do
            l <- gets (M.lookup n . funlbl)
            if isJust l then (return.fromJust) l else newlbl >>= addFunLbl n

          init  = Conv {regno = 3, lbl = 0, regmap = M.empty, funlbl = M.empty} -- FIXME: remove the hardcode


          mov (R a) (R b) dsc | a == b = []
          mov a b dsc = [opc (MOV a b) dsc]


