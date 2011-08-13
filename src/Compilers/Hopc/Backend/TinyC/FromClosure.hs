module Compilers.Hopc.Backend.TinyC.FromClosure where

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.Closure
import Compilers.Hopc.Backend.TinyC.IR

import Data.List
import Control.Monad
import Data.Generics.Biplate

import Text.Printf
import Debug.Trace

convert  :: Closure -> CompileM IR
convert k = trace "TRACE: FromClosure :: convert " $ do
    
    let morphed = tr k 

    trace ("TRACE: morphed \n" ) $ return ()

    return $ IR morphed
    
    where 
          tr :: Closure -> [Instr]

          tr (CLet n e e1) = trB (n,e) ++ tr e1
          tr (CLetR binds e) = concatMap trB binds ++ tr e
          tr (CApplCls n args) = [op $ CALL n ""]
          tr (CApplDir n args) = [op $ CALL n ""]
          tr (CMakeCls n args) = [op $ NOP]
          tr (CVar n)          = [opc (MOV (R 3) (R 0)) (printf "%s -> R0" n)]

          tr (CCond n e1 e2) = 
            let (ll1@(LABEL l1), c1) = (LABEL "L1", tr e1)
                (ll2@(LABEL l2), c2) = (LABEL "L2", tr e2)
            in op (CJUMP (JumpFake (R 0)) l1) : c2 ++ op (JUMP "L3") : (op ll1) : c1 ++ op (LABEL "L3") : []
            -- l1 : ll1 : JUMP  : c1 ++ ll2 : c2
            -- [CJUMP (JumpFake (R 0)) "" ""] -- error "got conditional"

          tr x = [op NOP]

          trB (n, (CInt v)) = [opc (CONST (show v) (R 0)) (printf "%d -> %s" v n)]
          trB (n, (CStr s)) = [opc (CONST s (R 0)) (printf "'%s' -> %s" s n)]
          trB (n, e)        = tr e ++ [opc (MOV (R 1) (R 0)) (printf " expr -> %s" n) ]


--          p (CApplDir n _ ) r = concat r ++ [CALL  n ""]
--          p (CLet n e _) r = trace ("TRACE: convert let " ++ n) $ bind (n, e) : concat r
--          p (CLetR b e) r = trace ("TRACE: convert letrec ") $ map bind b ++ concat r 
--          p x r = trace ("TRACE: convert " ++ show x) $ NOP : concat r

--          bind (n, CInt v) = CONST (show v) (R 0)
--          bind (n, CStr v) = CONST v (R 0)
--          bind (n, CVar _) = MOV (R 0) (R 0)
--          bind (n, CApplDir _ _) = MOV (R 0) (R 0)
--          bind (n, CApplCls _ _) = MOV (R 0) (R 0)
--          bind (n, x)            = MOV (R 10) (R 0) -- error $ "wtf? " ++ n ++ " "  ++ show x

