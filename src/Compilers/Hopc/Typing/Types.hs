module Compilers.Hopc.Typing.Types where

import Data.Maybe

type TypeId = String

class TType a where
    occurs    :: TypeId -> a -> Bool
    subst     :: TypeId -> a -> a -> a
    isVar     :: a -> Bool
    merge     :: a -> a -> Maybe [(a,a)]
    typeid    :: a -> TypeId

