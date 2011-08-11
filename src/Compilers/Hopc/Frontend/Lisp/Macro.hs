{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Frontend.Lisp.Macro where

import Prelude hiding (exp)
import Compilers.Hopc.Compile

import Compilers.Hopc.Frontend.Lisp.KNormalize (toString)
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.Lisp.MacroExpand

import Control.Monad

import Debug.Trace

expand :: TopLevel -> CompileM TopLevel 
expand (TopLevel e) = do
    e' <- mapM exp e
    return $ TopLevel e'

exp (ELet a b c e1 d e2 f) = do
    e1' <- exp e1
    e2' <- exp e2
    return $ ELet a b c e1' d e2' f

exp (ELetM o0 o binds c1 e2 c) = do
    binds' <- mapM expBind binds
    e2' <- exp e2
    return $ ELetM o0 o binds' c1 e2' c

exp  (ELambda o0 o atomts c1 e c) = do
    e' <- exp e
    return $ ELambda o0 o atomts c1 e' c

exp m@(EApply p1 (EAtom (AtomT (p12, "$$"))) args p2) = runMacro m

exp (EApply p1 (EAtom (AtomT (p12, bs))) args p2) = do
    args' <- mapM exp args
    return (EApply p1 (EAtom (AtomT (p12, bs))) args' p2)

exp (EDef def) = expDef def >>= return . EDef

exp x = return x

expBind (EBind o atomt e c) = do
    e' <- exp e
    return $ EBind o atomt e' c

expDef :: Def -> CompileM Def 
expDef (DefExp o atomt e c) = do 
    e' <- exp e
    return $ DefExp o atomt e c

expDef (DeFun o0 o atomt atomts c1 es c) = do
    es' <- mapM exp es
    return $ DeFun o0 o atomt atomts c1 es' c


