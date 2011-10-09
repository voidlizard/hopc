{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable  #-}
module Compilers.Hopc.Frontend.Types where

import Data.Data
import Data.Typeable

import Compilers.Hopc.Id (KId)
import Compilers.Hopc.Typing.Types

import Debug.Trace
import Text.Printf

data TFunSpec = TFunForeign Bool KId | TFunLocal deriving (Eq, Show, Ord, Data, Typeable)

data HType = TVar TypeId | TAny TypeId
             | TInt | TStr | TBool | TUnit | TFun TFunSpec [HType] HType
             | TAppl TypeId
             deriving (Show, Eq, Ord, Data, Typeable)


--instance Compilers

instance TType HType  where

    occurs t p@(TFun _ args r) = occursList t (r:args)
    occurs r (TAppl s) = r == s
    occurs t x = False

    subst ta a x@(TVar s) = if ta == s then a else x
    subst ta a (TFun s args r) = TFun s (substList ta a args) (subst ta a r)

--    subst ta a x@(TAppl s) = trace ("SUBST " ++ show (ta,a,x)) $ if ta == s then a else x

    subst ta a x = x

    isVar (TAppl _) = True 
    isVar (TVar _)  = True 
    isVar (TAny _)  = True
    isVar x         = False

    merge x@(TFun _ args1 r1) y@(TFun _ args2 r2) = Just $ (zip args1 args2) ++ [(r1, r2)] -- FIXME: what if different num of variables?
    merge x y = Nothing

    typeid (TVar  s)  = s
    typeid (TAny  s)  = s
    typeid (TAppl s)  = "appl_" ++ s
    typeid x         = "unknown"

isVarT :: HType -> Bool
isVarT (TVar _) = True
isVarT (TAppl _) = True
isVarT (TFun _ a b) = foldl (||) False $ map isVarT (b:a)
isVarT x = False

