{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Frontend.Lisp.MacroExpand (expand) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.Types
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs

import Debug.Trace

-- Just a stub for the future macro system
expand :: Exp -> CompileM Exp
expand (EMacro1 o e c) = do
    let (h:t) = map atom e
    dispatch h t
    return $ EUnit o c

expand _ = error "unexpected AST at MacroExpand.expand"

dispatch :: String -> [String] -> CompileM ()
dispatch "ccall" args = ccall args
dispatch x args = trace ("TRACE: UNKNOWN MACRO " ++ (show x)  ++ " " ++ (show args)) $ return ()


ccall :: [String] -> CompileM ()
ccall (nm:l:args) = do
    let tx = map typeis (l:args)
    let rt = last tx
    let at = (reverse . tail . reverse) tx
    addEntry nm (TFun (TFunForeign nm) at rt)

ccall x = trace "TRACE: BAD C-CALL" $ return () -- raise warning?

typeis ":int"    = TInt
typeis ":string" = TStr
typeis ":unit"   = TUnit
typeis ":bool"   = TBool
typeis x         = TVar x

atom (EAtom (AtomT (_, bs))) = toString bs
atom (EStr x) = x
atom (EInt x) = show x
atom _ = ""

toString = BS.unpack

