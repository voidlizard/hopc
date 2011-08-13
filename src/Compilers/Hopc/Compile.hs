{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Compilers.Hopc.Compile where

import qualified Data.Map as M
import qualified Data.Set as S

import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Types

import Data.Typeable
import Control.Monad ()
import Control.Monad.State
import Control.Monad.Error
import Control.Exception

data Entry = Entry { eType :: HType } 

type Dict = M.Map KId Entry 

data CompileState = CompileState Dict

newtype CompileM a = CompileM {
    runT :: (StateT CompileState (ErrorT CompileError IO)) a
} deriving (Monad, MonadIO, MonadError CompileError, MonadState CompileState)

--runCompile :: CompileState -> CompileM () -> CompileM (IO (Either CompileError, a))
runCompile :: CompileState
              -> CompileM a
              -> IO (Either CompileError (a, CompileState))

runCompile init f = runErrorT (runStateT (runT f) init)

initCompile = CompileState M.empty

addEntry :: KId -> HType -> CompileM ()
addEntry n t = do 
    (CompileState d) <- get
    let d' = M.insert n (Entry {eType = t}) d
    put (CompileState d')

getEntry :: KId -> CompileM (Maybe Entry)
getEntry n = do
    (CompileState d) <- get
    return $ M.lookup n d

names :: CompileM (S.Set KId)
names = do
    (CompileState d) <- get
    return $ M.keysSet d

