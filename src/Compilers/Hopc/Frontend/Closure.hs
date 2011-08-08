{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module Compilers.Hopc.Frontend.Closure where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Data
import Data.Typeable
import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass

import Text.Printf
import Debug.Trace

import Compilers.Hopc.Frontend.KTree

data Fun = Fun KId [KId] [KId] Closure deriving (Show, Eq, Data, Typeable)

data Closure =  CInt Integer
              | CStr String
              | CUnit
              | CLet  KId Closure Closure
              | CLetR [(KId, Closure)] Closure
              | CVar KId
              | CFun Fun
              | CMakeCls KId
              | CApplCls KId [KId]
              | CApplDirect KId [KId] 
             deriving (Show, Eq, Data, Typeable)


data Conv = Conv { fns :: [(KId, Fun)], known :: S.Set KId } deriving (Show)

type ConvM = State Conv

convert :: KTree -> Closure
convert k = 
    let (cls, s) = runState (conv k) convInit
        binds = bindsOfCls cls
    in CLetR (map bindsOfFn (fns s) ++ binds) (cOfCls cls)
    where bindsOfCls (CLet n e1 e2) = [(n, e1)]
          bindsOfCls (CLetR binds e2) = binds
          bindsOfCls x = []
          cOfCls (CLet _ _ e) = e
          cOfCls (CLetR _ e) = e
          cOfCls e = e
          bindsOfFn (n, f@(Fun nm args free c)) = (fname n, CFun f)
--            let free' = filter (not.(flip elem args)) $ [n | CVar n <- universe c] ++ [n | CApplCls n _ <- universe c]
--            in (fname n, f) --CFun (Fun nm args free' c))

conv KUnit = return CUnit 
conv (KInt n) = return $ CInt n
conv (KStr s) = return $ CStr s
conv (KVar n) = return $ CVar n

conv (KLet n e1 e2) = do
    (n', e1') <- convBind (n, e1)
    e2' <- conv e2
    return $ CLet n e1' e2'

conv (KLetR binds e2) = do 
    binds' <- forM binds convBind
    e2' <- conv e2
    return $ CLetR binds' e2'

conv (KApp n args) = do
    return $ CApplCls n args

conv wtf = error $ "WTF? " ++ show wtf

convBind (n, e@(KLambda args eb)) = do
    addKnown n
    mapM_ addKnown args
    e' <- conv eb
    fv <- freeVars args eb
    addFun n args fv e'
    return $ (n, CMakeCls (fname n))

convBind (n, e) = do
    addKnown n
    e' <- conv e
    return $ (n, e')

convInit = Conv [] S.empty

isKnown :: KId -> ConvM Bool
isKnown n = gets (\(Conv { known = kn }) -> S.member n kn )

addKnown :: KId -> ConvM ()
addKnown n = do
    s@(Conv { known = k }) <- get
    put $ s { known = S.insert n k }

addFun :: KId -> [KId] -> [KId] -> Closure -> ConvM ()
addFun n args free bdy = do
    s@(Conv { fns = fs }) <- get
    put $ s { fns = fs ++ [ (n, Fun (fname n) args free bdy )] }

fname n = "fun_" ++ n

freeVars args cls = do
    let v1 = [n | KVar n   <- universe cls]
    let v2 = [n | KApp n _ <- universe cls]
    knowns <- filterM isKnown (v1 ++ v2)
    return $ filter (not.(flip elem args) ) knowns

instance Pretty Closure where
    pPrintPrec _ _ (CUnit)  = text "()" 
    pPrintPrec _ _ (CInt n) = integer n 
    pPrintPrec _ _ (CStr s) = (text.show) s
    pPrintPrec _ _ (CVar v) = text v
    pPrintPrec l p (CFun (Fun n args free e)) = prettyParen True $ text "func"
                                                <+> (prettyParen True (fsep $ map text (args++free) ))
                                                <+> pPrintPrec l p e
    pPrintPrec l p (CApplCls n a) = prettyParen True $ text "apply-closure" <+> text n <+> ( fsep $ map text a )
    pPrintPrec l p (CApplDirect n a) = prettyParen True $ text "apply-direct" <+> text n <+> ( fsep $ map text a )
    pPrintPrec l p (CMakeCls s) = prettyParen True $ text "make-closure" <+> text s 
    pPrintPrec l p (CLet i e1 e2) = prettyParen True $ text "let"
                                    <+> (prettyParen True $ text i <+> pPrintPrec l p e1)
                                    $$ nest 2 (pPrintPrec l p e2)
    pPrintPrec l p (CLetR binds e) = prettyParen True $ text "letrec"
                                     <+> prettyParen True ( fsep $ map (\(n, e1) -> prettyParen True (text n <+> pPrintPrec l p e1)) binds )
                                     $$ nest 2 (pPrintPrec l p e)

