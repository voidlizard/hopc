module Compilers.Hopc.Frontend.KTree (KTree(..), KTreeModule) where

import Control.Monad.Error

type KTreeModule = [KTree]

data KTree = KInt Integer deriving Show

