{-# LANGUAGE EmptyDataDecls #-}
module Compilers.Hopc.Frontend.Types where

import Compilers.Hopc.Frontend.KTree (KId)
import Compilers.Hopc.Typing.Types

import Debug.Trace

data TFunSpec = TFunForeign KId | TFunLocal deriving (Eq, Show)

data HType = TVar TypeId | TInt | TStr | TBool | TUnit | TFun TFunSpec [HType] HType
             | TAppl TypeId
             deriving (Eq, Show)



--instance Compilers

instance TType HType  where

    occurs t (TFun _ args r) = occursList t args && occurs t r
    occurs r (TAppl s) = r == s
    occurs t x = False

    subst ta a x@(TVar s) = if ta == s then a else x
    subst ta a (TFun s args r) = TFun s (substList ta a args) (subst ta a r)

    subst ta a x@(TAppl s) = if ta == s then a else x

    subst ta a x = x

    isVar (TAppl _) = False 
    isVar (TVar _)  = True 
    isVar x         = False

    merge (TFun _ args1 r1) (TFun _ args2 r2) = Just $ (zip args1 args2) ++ [(r1, r2)]
--    merge (TFun _ args r1) (TAppl x) = Nothing -- Just $ [(TVar x, r1)]

--    merge x y = trace ("TRACE: merge " ++ (show x) ++ " " ++ (show y)) $ Nothing
    merge x y = trace ("TRACE: merge " ++ (show x) ++ " " ++ (show y)) $ Nothing

    typeid (TVar  s)  = s
    typeid (TAppl s)  = s
    typeid x         = "unknown"


isVarT :: HType -> Bool
isVarT (TVar _) = True
isVarT (TAppl _) = True
isVarT (TFun _ a b) = foldl (||) False $ map isVarT (b:a)
isVarT x = False

