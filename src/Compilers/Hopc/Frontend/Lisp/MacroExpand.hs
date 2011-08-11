{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Frontend.Lisp.MacroExpand where

import Control.Monad

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.Lisp.KNormalize (toString)
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs

import Debug.Trace

runMacro :: Exp -> CompileM Exp

runMacro (EApply p1 (EAtom (AtomT (p12, "$$"))) args p2) = do
    let vals = map atom args
    trace "GOT $$-MACRO" $ return ()
--    liftIO $ putStrLn "GOT MACRO"
    return $ EUnit p1 p2

runMacro e = return e

atom (EAtom (AtomT (_, bs))) = toString bs
atom (EStr x) = x
atom (EInt x) = show x
atom _ = ""
