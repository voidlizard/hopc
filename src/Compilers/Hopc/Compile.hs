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

data CompileState = CompileState Dict deriving (Show)

newtype CompileM a = CompileM {
    runT :: (StateT CompileState (ErrorT CompileError IO)) a
} deriving (Monad, MonadIO, MonadError CompileError, MonadState CompileState)

--runCompile :: CompileState -> CompileM () -> CompileM (IO (Either CompileError, a))
runCompile :: CompileState
              -> CompileM a
              -> IO (Either CompileError (a, CompileState))

runCompile init f = runErrorT (runStateT (runT f) init)

initCompile = CompileState M.empty

addEntry :: Bool -> KId -> HType -> CompileM ()
addEntry tp n t = do 
    (CompileState d) <- get
    let d' = M.insert n (Entry {eType = t, eTop = tp}) d
    put (CompileState d')

addEntries :: Bool -> [(KId, HType)] -> CompileM ()
addEntries t ls = do
    let dict = M.fromList (map (\(a, b) -> (a, Entry b t)) ls)
    modify ( \(CompileState d) -> CompileState (M.union d dict))

getEntry :: KId -> CompileM (Maybe Entry)
getEntry n = do
    (CompileState d) <- get
    return $ M.lookup n d

getEntryType :: KId -> CompileM (Maybe HType)
getEntryType n = do
    e <- getEntry n
    return $ maybe Nothing (Just . eType) e

getEntries :: CompileM (M.Map KId HType)
getEntries = do 
    (CompileState d) <- get
    return $ M.map eType d


entryList :: CompileM [(KId,Entry)]
entryList = do 
    (CompileState d) <- get
    return $ M.toList d

names :: CompileM (S.Set KId)
names = do
    (CompileState d) <- get
    return $ M.keysSet d

