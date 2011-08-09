{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compilers.Hopc.Frontend.Lisp.KNormalize where

import qualified Data.ByteString.Char8 as BS
import Text.Printf

import Control.Monad.State

import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.KTree

toString :: BS.ByteString -> String
toString = BS.unpack

kNormalizeExp :: Exp -> KTree
kNormalizeExp e = evalState (knorm e) kInitState

kNormalizeTop :: TopLevel -> KTree
kNormalizeTop (TopLevel c) = flip evalState kInitState (knormSeq c)

knormTop :: Exp -> KNormStateM (KId, KTree)

knormTop (EDef (DeFun _ _ (AtomT (p,bs)) args _ es _)) = do
    bdy <- forM es $ \e -> undefined
    undefined

knormTop (EDef (DefExp _ (AtomT (p,bs)) e _)) = do
    e' <- knorm e
    return (toString bs, e')

knormTop e = do
    t <- tmp "" "tmp"
    e' <- knorm e
    return $ (t, e')

isDef (EDef _) = True
isDef x        = False

knormSeq :: [Exp] -> KNormStateM KTree

knormSeq [] = return KUnit

knormSeq seq = do
    let (last:all) = reverse seq
    es <- mapM knormN (reverse all)
    (t, last') <- knormN last
    lastNorm <- knormTail last'
    if es == []
        then return $ if isDef last then KLetR [(t, last')] KUnit else lastNorm
        else return $ if isDef last then KLetR (es++[(t, last')]) KUnit else KLetR es lastNorm 

    where knormTail :: KTree -> KNormStateM KTree
          knormTail l@(KLambda args k) = do
            n <- tmp "l" "tmp"
            return $ KLet n l (KVar n) 

          knormTail x = return x 

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

withArg (AtomT (p, bs)) = (toString bs)
withArg x = error "withArg"

knormN :: Exp -> KNormStateM (KId, KTree)

knormN (EDef (DeFun _ _ (AtomT (p,bs)) args _ e _)) = do
    seq <- knormSeq e
    let args' = map withArg args
    let fn = toString bs
    return $ (fn, KLambda args' seq)

knormN (EDef (DefExp _ (AtomT (p,bs)) e _)) = do
    let s = toString bs
    e' <- knorm e
    return (s, e')

knormN e = do
    s  <- tmp "" "tmp"
    e' <- knorm e
    return (s, e')

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

knorm (ELetM p1 p2 binds p3 e p4) = do
    binds' <- forM binds $ \(EBind _ (AtomT (_, bs)) eb _) -> 
                do let t  = toString bs
                   eb' <- knorm eb
                   return (t, eb')
    e' <- knorm e
    return $ KLetR binds' e'

knorm (ELet p1 p2 (AtomT (p21, bs)) eb p3 e p4) = do
    eb' <- knorm eb
    e'  <- knorm e
    let tmpname = toString bs
    return $ KLet tmpname eb' e'

knorm (EApply p1 (EAtom (AtomT (p12, bs))) args p2) = do
    let fn = toString bs
    knormApp fn args

knorm (EApply p1 e args p2) = do
    (fn, e')  <- knormN e
    app <- knormApp fn args
    return $ KLet fn e' app

knorm (EDef (DeFun _ _ (AtomT (p,bs)) args _ e _)) = do
    seq <- knormSeq e
    let args' = map withArg args
    let fn = toString bs
    return $ KLambda args' seq

knorm (EDef (DefExp _ (AtomT (p,bs)) e _)) = do
    let s = toString bs
    e' <- knorm e
    return e'

knorm x = error $ "wtf? " ++ show x

knormApp fn a = do
    parts <- mapM ofArg a
    return $ foldr (\x acc -> (snd x) acc) (KApp fn (map fst parts)) parts
    where ofArg e = do
            t   <- tmp "" "tmp"
            en  <- knorm e
            return $ (t, KLet t en)


