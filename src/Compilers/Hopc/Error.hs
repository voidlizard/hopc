{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Compilers.Hopc.Error where

import Data.Typeable
import Control.Monad ()
import Control.Monad.State
import Control.Monad.Error
import Control.Exception

data CompileError = GeneralError | ParseError String  deriving (Show, Typeable)

instance Exception CompileError 
instance Error CompileError 
