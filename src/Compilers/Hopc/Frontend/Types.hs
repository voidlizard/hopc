{-# LANGUAGE EmptyDataDecls #-}
module Compilers.Hopc.Frontend.Types where

import Compilers.Hopc.Frontend.KTree (KId)
import Compilers.Hopc.Typing.Types

data TFunSpec = TFunForeign KId | TFunLocal deriving (Eq, Show)

data HType = TVar TypeId | TInt | TStr | TBool | TUnit | TFun TFunSpec [HType] HType
             deriving (Eq, Show)


instance TType HType  where

    occurs t (TFun _ args r) = occursList t args && occurs t r
    occurs t x = False

    subst ta a x@(TVar s) = if ta == s then a else x
    subst ta a (TFun s args r) = TFun s (substList ta a args) (subst ta a r)
    subst ta a x = x

    isVar (TVar _)  = True
    isVar x         = False

    merge (TFun _ args1 r1) (TFun _ args2 r2) = Just $ (zip args1 args2) ++ [(r1, r2)]
    merge x y = Nothing

    typeid (TVar s)  = s
    typeid x         = "unknown"

