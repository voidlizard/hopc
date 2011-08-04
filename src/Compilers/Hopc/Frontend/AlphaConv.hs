module Compilers.Hopc.Frontend.AlphaConv where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Generics.Biplate

import Compilers.Hopc.Frontend.KTree

data AlphaConv = AlphaConv { aId :: Int, aEnv :: M.Map KId KId } deriving Show

type AlphaConvM = State AlphaConv

alphaConv :: KTree -> KTree
alphaConv k = evalState (descendBiM tr k) aInitState
    where tr :: KTree -> AlphaConvM KTree

          tr (KLet s e1 e2) = do
            sn <- replVar s
            e1' <- tr e1
            e2' <- tr e2
            return $ KLet sn e1' e2'

          tr (KLetR binds e2) = do
            sn  <- mapM (replVar . fst) binds
            e1  <- mapM (tr . snd) binds
            e2' <- tr e2
            return $ KLetR (zip sn e1) e2'

          tr (KLambda args e) = do
            vars <- mapM replVar args
            e'   <- tr e
            return $ KLambda vars e'

          tr (KVar s) = getVar s >>= return . KVar

          tr (KApp fn args) = do
            fn'   <- getVar fn
            args' <- mapM getVar args
            return $ KApp fn' args'

          tr x = return x

aInitState = AlphaConv { aId = 0, aEnv = M.empty }

getVar :: KId -> AlphaConvM KId
getVar s = do
    st@(AlphaConv {aEnv = env}) <- get
    return $ maybe s id $ M.lookup s env

replVar :: KId -> AlphaConvM KId
replVar s = do
    AlphaConv {aId = n, aEnv = env} <- get
    let nv = s ++ "_" ++ show n
    put AlphaConv { aId = n+1, aEnv = M.insert s nv env }
    return nv

