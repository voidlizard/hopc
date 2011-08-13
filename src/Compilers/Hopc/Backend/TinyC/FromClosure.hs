module Compilers.Hopc.Backend.TinyC.FromClosure where

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.Closure
import Compilers.Hopc.Backend.TinyC.IR

import Data.List
import Control.Monad
import Data.Generics.Biplate

import Debug.Trace

convert  :: Closure -> CompileM IR
convert k = trace "TRACE: FromClosure :: convert " $ do
    
    let morphed = para p k

    trace ("TRACE: morphed " ++ intercalate "\n" (map show morphed)) $ return ()

    return $ IR []
    
    where 
          
          p (CApplDir n _ ) r = concat r ++ [CALL  n ""]
          p (CLet n (CInt v) _) r = trace ("TRACE: convert let " ++ n) $ (CONST (show v) (R 0)) : concat r
          p (CLetR n e) r = trace ("TRACE: convert letrec ") $ NOP : concat r
          p x r = trace ("TRACE: convert " ++ show x) $ NOP : concat r

