{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Compilers.Hopc.Compile where

import qualified Data.Map as M

import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Types

import Data.Typeable
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Exception

data Entry = Entry { eType :: HType } 

type Dict = M.Map KId Entry 

data CompileError = GeneralError deriving (Show, Typeable)

instance Exception CompileError 
instance Error CompileError 

data CompileState = CompileState Dict

--newtype CompileM a = CompileM {
--    runT :: (StateT CompileState (ErrorT   (ReaderT TerminalInit (StateT TerminalState (ErrorT CardReaderError IO))) a
--} deriving (Monad, MonadIO, MonadError CardReaderError, MonadReader TerminalInit, MonadState TerminalState)


