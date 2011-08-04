{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compilers.Hopc.Frontend.Lisp.KNormalize where

import qualified Data.ByteString.Char8 as BS
import Text.Printf

import Control.Monad.State

import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
--import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Par
import Compilers.Hopc.Frontend.KTree

toString :: BS.ByteString -> String
toString = BS.unpack

kNormalizeExp :: Exp -> KTree
kNormalizeExp e = evalState (knorm e) kInitState

data KNormState = KNormState { tmpId :: Int }

type KNormStateM = State KNormState

kInitState :: KNormState
kInitState = KNormState { tmpId = 0 }

tmp :: String -> String -> KNormStateM String 
tmp prefix s = do
    (KNormState {tmpId = i}) <- get
    let v = printf "%s_%s%d" prefix s i
    put KNormState { tmpId = i+1 }
    return v

knorm :: Exp -> KNormStateM KTree

knorm (EUnit _ _) = return $ KUnit

knorm (EInt i) = return $ KInt i

knorm (EStr s) = return $ KStr s

knorm (EAtom (AtomT (p,bs))) = return $ KVar (toString bs)

--knorm (EList _ _ _ _) = error "List literals are not supported yet"

knorm (ELambda _ _ args _ e _) = do
    let args' = map withArg args
    e' <- knorm e
    return $ KLambda args' e'
    where  withArg (AtomT (p, bs)) = (toString bs) 

knorm (ELet p1 p2 (AtomT (p21, bs)) eb p3 e p4) = do
    eb' <- knorm eb
    e'  <- knorm e
    let tmpname = toString bs
    return $ KLet tmpname eb' e'

knorm (EApply p1 (EAtom (AtomT (p12, bs))) args p2) = do
    let fn = toString bs
    knormApp fn args

knorm (EApply p1 e args p2) = do
    e'  <- knorm e
    fn  <- tmp "" "fun"
    app <- knormApp fn args
    return $ KLet fn e' app

knormApp fn a = do
    parts <- mapM ofArg a
    return $ foldr (\x acc -> (snd x) acc) (KApp fn (map fst parts)) parts
    where ofArg e = do
            t   <- tmp "" "tmp"
            en  <- knorm e
            return $ (t, KLet t en)

