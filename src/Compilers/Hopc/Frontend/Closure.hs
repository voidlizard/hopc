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


data Conv = Conv { fns :: M.Map KId Fun, known :: S.Set KId, free :: S.Set KId } deriving (Show)

type ConvM = State Conv

convert :: KTree -> Closure
convert k = 
    let (cls, s) = runState (conv k) convInit
        binds = bindsOfCls cls
    in CLetR (map bindsOfFn (M.toList (fns s)) ++ binds) (cOfCls cls)
    where bindsOfCls (CLet n e1 e2) = [(n, e1)]
          bindsOfCls (CLetR binds e2) = binds
          bindsOfCls x = []
          cOfCls (CLet _ _ e) = e
          cOfCls (CLetR _ e) = e
          cOfCls e = e
          bindsOfFn (n, f@(Fun nm args free c)) = (fname n, CFun f)

conv KUnit = return CUnit 
conv (KInt n) = return $ CInt n
conv (KStr s) = return $ CStr s

conv (KVar n) = addFree n >> (return . CVar) n

conv (KLet n e1 e2) = do
    (n', e1') <- convBind (n, e1)
    e2' <- conv e2
    return $ CLet n e1' e2'

conv (KLetR binds e2) = do
    binds' <- forM binds convBind
    e2' <- conv e2
    return $ CLetR binds' e2'

conv (KApp n args) = addFree n >> (return $ CApplCls n args)

conv wtf = error $ "WTF? " ++ show wtf

convBind (n, e@(KLambda args eb)) = do
    addKnown n
    knowns' <- gets known
    let knowns = S.difference knowns' (S.fromList args)
    st@(Conv { fns = fs }) <- get
    trace ("KNOWNS " ++ n ++ " " ++ show knowns ++ " ARGS " ++ show args) $ do
        let (e', s') = runState (conv eb) (st {known = knowns})
        st <- put $ st { fns = M.insert n (funOf n args ((S.toList.free) s') e') fs `M.union` (fns s') }
        return $ (n, CMakeCls (fname n))
--        trace ("FREE " ++ n + " " ++ (show (free s')) $ do
--        addFun n args ((S.toList.free) s') e'

convBind (n, e) = do
    addKnown n
    e' <- conv e
    return $ (n, e')

convInit = Conv M.empty S.empty S.empty

addFree :: KId -> ConvM ()
addFree n = do
    knowns <- gets known
    if S.member n knowns
        then addFree' n
        else return ()
    where  addFree' :: KId -> ConvM () 
           addFree' n = do
               st@(Conv { free = fr }) <- get
               put $ st { free = S.insert n fr }

addKnown n = do
    s@(Conv { known = kn }) <- get
    put $ s { known = S.insert n kn }

addFun :: KId -> [KId] -> [KId] -> Closure -> ConvM ()
addFun n args free bdy = do
    s@(Conv { fns = fs }) <- get
    put $ s { fns = M.insert n (funOf n args free bdy) fs }

funOf n args free bdy = (Fun (fname n) args free bdy)

fname n = "fun_" ++ n

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

