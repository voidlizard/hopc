module Compilers.Hopc.Frontend.KTree (KTree(..), KTreeModule) where

import Control.Monad.Error

type KId = String

type KTreeModule = [KDef]

data KDef = KFun KId KFunArgs KTree

data KFunArgs = Int

data KTree = KInt Integer deriving Show

