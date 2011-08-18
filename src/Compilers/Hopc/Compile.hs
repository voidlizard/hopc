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

type TDict = M.Map KId HType

data CompileState = CompileState { cdict :: Dict, centry :: Maybe KId, ctmpid :: Int }
                    deriving (Show)

newtype CompileM a = CompileM {
    runT :: (StateT CompileState (ErrorT CompileError IO)) a
} deriving (Monad, MonadIO, MonadError CompileError, MonadState CompileState)

--runCompile :: CompileState -> CompileM () -> CompileM (IO (Either CompileError, a))
runCompile :: CompileState
              -> CompileM a
              -> IO (Either CompileError (a, CompileState))

runCompile init f = runErrorT (runStateT (runT f) init)

initCompile = CompileState M.empty Nothing 0

retvalVariable = "RETVAL"
activationRecordVariable = "CLOSURE"

addEntry :: Bool -> KId -> HType -> CompileM ()
addEntry tp n t = do 
    c@(CompileState { cdict = d }) <- get
    put c {cdict = M.insert n (Entry {eType = t, eTop = tp}) d}

addEntries :: Bool -> [(KId, HType)] -> CompileM ()
addEntries t ls = do
    let dict = M.fromList (map (\(a, b) -> (a, Entry b t)) ls)
    modify ( \s@(CompileState { cdict = d }) -> s {cdict = M.union d dict})

getEntry :: KId -> CompileM (Maybe Entry)
getEntry n = gets ( M.lookup n . cdict )

getEntryType :: KId -> CompileM (Maybe HType)
getEntryType n = do
    e <- getEntry n
    return $ maybe Nothing (Just . eType) e

getEntries :: CompileM (M.Map KId HType)
getEntries = gets ((M.map eType).cdict)

getConstraints :: CompileM [(HType, HType)]
getConstraints = do
    e <- getEntries 
    return $ map (\(a,b) -> (TVar a, b)) $ M.toList e

entryList :: CompileM [(KId,Entry)]
entryList = gets (M.toList.cdict)

names :: CompileM (S.Set KId)
names = gets (M.keysSet . cdict)

setEntryPoint :: KId -> CompileM ()
setEntryPoint s = modify (\cs -> cs { centry = Just s })

getEntryPoint :: CompileM (Maybe KId)
getEntryPoint = gets centry

nextTmp :: CompileM Int
nextTmp = do
    t <- gets ctmpid
    modify (\s@(CompileState{ctmpid = n}) -> s {ctmpid = succ n})
    return t


