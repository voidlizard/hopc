module Compilers.Hopc.Frontend.AlphaConv where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Generics.Biplate

import Compilers.Hopc.Compile
import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.KTree

data AlphaConv = AlphaConv { aId :: Int, aEnv :: M.Map KId KId } deriving Show

type AlphaConvM = State AlphaConv

alphaConvM :: KTree -> CompileM KTree
alphaConvM = return . alphaConv

alphaConv :: KTree -> KTree
alphaConv k = evalState (descendBiM tr k) aInitState
    where tr :: KTree -> AlphaConvM KTree

          tr (KLet s e1 e2) = do
            e1' <- tr e1
            sn <- replVar s
            e2' <- tr e2
            return $ KLet sn e1' e2'

          tr (KLetR binds e2) = do
            forM_ binds (replVar.fst)

            binds' <- forM binds $ \(s, e1) -> do
                s'  <- getVar s
                e1' <- tr e1
                return (s', e1')

            e2' <- tr e2
            return $ KLetR binds' e2'

          tr (KLambda args e) = do
            st@(AlphaConv {aId = aid, aEnv = aenv}) <- get
            let (v, s) = flip runState st $ do
                vars <- mapM replVar args
                e'   <- tr e
                return $ KLambda vars e'
            put st{aId = aId s}
            return v

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

