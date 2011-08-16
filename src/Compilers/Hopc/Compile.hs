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

data Entry = Entry { eType :: HType, eTop :: Bool } deriving (Show)

type Dict = M.Map KId Entry 

data CompileState = CompileState Dict (Maybe KId) deriving (Show)

newtype CompileM a = CompileM {
    runT :: (StateT CompileState (ErrorT CompileError IO)) a
} deriving (Monad, MonadIO, MonadError CompileError, MonadState CompileState)

--runCompile :: CompileState -> CompileM () -> CompileM (IO (Either CompileError, a))
runCompile :: CompileState
              -> CompileM a
              -> IO (Either CompileError (a, CompileState))

runCompile init f = runErrorT (runStateT (runT f) init)

initCompile = CompileState M.empty Nothing

addEntry :: Bool -> KId -> HType -> CompileM ()
addEntry tp n t = do 
    (CompileState d e) <- get
    let d' = M.insert n (Entry {eType = t, eTop = tp}) d
    put (CompileState d' e)

addEntries :: Bool -> [(KId, HType)] -> CompileM ()
addEntries t ls = do
    let dict = M.fromList (map (\(a, b) -> (a, Entry b t)) ls)
    modify ( \(CompileState d e) -> CompileState (M.union d dict) e)

getEntry :: KId -> CompileM (Maybe Entry)
getEntry n = do
    (CompileState d _) <- get
    return $ M.lookup n d

getEntryType :: KId -> CompileM (Maybe HType)
getEntryType n = do
    e <- getEntry n
    return $ maybe Nothing (Just . eType) e

getEntries :: CompileM (M.Map KId HType)
getEntries = do 
    (CompileState d _) <- get
    return $ M.map eType d

getConstraints :: CompileM [(HType, HType)]
getConstraints = do
    e <- getEntries 
    return $ map (\(a,b) -> (TVar a, b)) $ M.toList e

entryList :: CompileM [(KId,Entry)]
entryList = do 
    (CompileState d _) <- get
    return $ M.toList d

names :: CompileM (S.Set KId)
names = do
    (CompileState d _) <- get
    return $ M.keysSet d

setEntryPoint :: KId -> CompileM ()
setEntryPoint s = do
    (CompileState d e) <- get
    put (CompileState d (Just s))

getEntryPoint :: CompileM (Maybe KId)
getEntryPoint = do
    (CompileState _ e) <- get
    return e

