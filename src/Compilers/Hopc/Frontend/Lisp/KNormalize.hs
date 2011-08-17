{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, DeriveDataTypeable #-}

module Compilers.Hopc.Frontend.Lisp.KNormalize where

import Compilers.Hopc.Compile
import qualified Data.ByteString.Char8 as BS
import Text.Printf

import qualified Data.Map as M
import Data.Maybe
import Data.Either
import Data.Data
import Data.Typeable
import Data.Generics.PlateData

import Control.Monad.State
import Control.Monad.Trans

import Compilers.Hopc.Compile
import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Types

import Debug.Trace

data KNormState = KNormState { tmpId :: Int }
type KNormStateM = StateT KNormState CompileM --KNormState -- (CompileM KNormState)

toString :: BS.ByteString -> String
toString = BS.unpack

kNormalizeExp :: Exp -> CompileM KTree
kNormalizeExp e = evalStateT (knormSeq [e]) kInitState

kNormalizeTop :: TopLevel -> CompileM KTree
kNormalizeTop (TopLevel c) = flip evalStateT kInitState (knormSeq c)

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
        then rewriteBiM r $ if isDef last then KLetR [(t, last')] KUnit else lastNorm
        else rewriteBiM r $ if isDef last then KLetR (es++[(t, last')]) KUnit else KLetR es lastNorm

    where knormTail :: KTree -> KNormStateM KTree
          knormTail l@(KLambda args k) = do
            n <- tmp "l" "tmp"
            return $ KLet n l (KVar n) 

          knormTail x = return x 

          r (KLet n e el@(KLambda _ _)) = do
            ln <- tmp "l" "tmp"
            return $ Just $ KLetR [(n,e), (ln, el)] (KVar ln)

          r (KLetR bs el@(KLambda _ _)) = do 
            ln <- tmp "l" "tmp"
            return $ Just $ KLetR (bs ++ [(ln, el)]) (KVar ln)

          r _  = return Nothing


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

knorm (EMacroT o atomt ttype c) = do
    let n = ofatom atomt
    let t = kTypeof ttype
    return $ KSpecial (KTypeDef (n, t))

knorm (EMacroFrn o str atomt ttypefun c) = do
    let n = ofatom atomt
    let t = kTypeofFn (Just str) ttypefun
    lift $ addEntry True n t
    return KUnit

--knorm m@(EMacroT (ETypeFunForeign o0 o1 str o atomt2 atomts c3 atomt c4 c)) = do
--    let n = str
--    let fn = ofatom atomt2
--    let tt = funtype (TFunForeign n) atomts atomt
--    when ((not.isJust) tt) $ error $ "BAD TYPE DECLARATION " ++ n -- FIXME
--    maybe (return ()) (lift . addEntry True fn) tt
--    e <- lift getEntries
--    liftIO $ forM_ (M.toList e) print

--knorm m@(EMacroT (ETypeFun o0 o1 o atomt2 atomts c3 atomt c4 c)) = do

--    let n = ofatom atomt2

--    let tt = funtype TFunLocal atomts atomt

--    when ((not.isJust) tt) $ error $ "BAD TYPE DECLARATION " ++ n -- FIXME
--    return $ KSpecial (KTypeDef (n, (fromJust tt)))

--knorm m@(EMacroT (ETypeVar o0 o atomt1 atomt c2 c)) = do
--    let n = ofatom atomt1
--    let tp = (typeofstr.ofatom) atomt
--    case tp of
--        (Left x) -> error $ "BAD TYPE DECLARATION " ++ (show x)
--        Right t  -> return $ KSpecial (KTypeDef (n, t))

--knorm (EList _ _ _ _) = error "List literals are not supported yet"

knorm (ELambda _ _ args _ e _) = do
    let args' = map withArg args
    e' <- knorm e
    return $ KLambda args' e'

--knorm (ELetM p1 p2 binds p3 el@(ELambda _ _ args _ e _) p4) = do
--    error "GOT LAMBDA"
--    el' <- knorm el
--    binds' <- forM binds $ \(EBind _ (AtomT (_, bs)) eb _) -> 
--                do let t  = toString bs
--                   eb' <- knorm eb
--                   return (t, eb')
--    e' <- knorm e
--    return $ KLetR binds' e'

knorm (ELetM p1 p2 binds p3 e p4) = do
    binds' <- forM binds $ \(EBind _ (AtomT (_, bs)) eb _) -> 
                do let t  = toString bs
                   eb' <- knorm eb
                   return (t, eb')
    e' <- knorm e
    return $ KLetR binds' e'

--knorm (ELet p1 p2 (AtomT (p21, bs)) eb p3 el@(ELambda _ _ args _ e _) p4) = do
--    error "GOT LAMBDA"
--    eb' <- knorm eb
--    e'  <- knorm e
--    let tmpname = toString bs
--    return $ KLet tmpname eb' e'

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

knorm (EBegin o exps c) = knormSeq exps

knorm (ECond o e e1 e2 c) = do
    t   <- tmp "" "tmp"
    t1  <- tmp "" "tmp"
    t2  <- tmp "" "tmp"
    e'  <- knorm e
    e1' <- knorm e1
    e2' <- knorm e2
    return $ KLet t e' (KCond t (KLet t1 e1' (KVar t1)) (KLet t2 e2' (KVar t2)))

knorm x = error $ "wtf? " ++ show x

knormApp fn a = do
    parts <- mapM ofArg a
    return $ foldr (\x acc -> (snd x) acc) (KApp fn (map fst parts)) parts
    where ofArg e = do
            t   <- tmp "" "tmp"
            en  <- knorm e
            return $ (t, KLet t en)

ofatom (AtomT (p, bs)) = toString bs

kTypeof :: TType -> HType

kTypeof (ETypeAtom atomt) = (typeofstr.ofatom) atomt

kTypeof (ETypeFun tf) = kTypeofFn Nothing tf

kTypeof _ = error "BAD TYPE DECL" -- FIXME

kTypeofFn :: Maybe String -> TTypeFun -> HType 

kTypeofFn native (ETypeFunDecl o (ETypeList _ ttypes _) ttype c) = 
    let at = map kTypeof ttypes
        rt = kTypeof ttype
        ft = case native of
               Nothing -> TFunLocal
               Just n  -> TFunForeign n
    in TFun ft at rt

kTypeofFn _ _ = error "BAD TYPE DECL" -- FIXME

typeofstr ":unit"   = TUnit
typeofstr ":string" = TStr
typeofstr ":bool"   = TBool
typeofstr ":int"    = TInt
typeofstr x         = (TAny x)

--funtype :: TFunSpec -> [AtomT] -> AtomT -> Maybe HType
--funtype spec atomts atomt = do
--    let (err1, at) = partitionEithers $ map (typeofstr.ofatom) atomts
--    let rt = typeofstr $ ofatom atomt

--    case (at, rt, err1) of
--        (x, Right r, []) -> Just $ TFun spec (x) r
--        _ -> Nothing

