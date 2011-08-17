module Compilers.Hopc.Frontend.AlphaConv where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Generics.Biplate

import Compilers.Hopc.Compile
import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Types

import Debug.Trace

data AlphaConv = AlphaConv { aId :: Int, aEnv :: M.Map KId KId, aTp :: M.Map KId HType } deriving Show

type AlphaConvM = StateT AlphaConv CompileM

alphaConv :: KTree -> CompileM KTree
alphaConv k = evalStateT (descendBiM tr k) aInitState

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
            (v, s) <- lift $ flip runStateT st $ do
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

          tr (KCond t e1 e2) = do
            t'   <- getVar t
            e1' <- tr e1
            e2' <- tr e2
            return $ KCond t' e1' e2'

          tr s@(KSpecial (KTypeDef (n, t))) = do
            st@(AlphaConv {aTp = tps}) <- get
            put st { aTp = M.insert n t tps}

            st2@(AlphaConv {aTp = tps}) <- get
            trace ("TRACE: KSpecial tps " ++ (show st2)) $ return ()

            return KUnit

          tr x = return x

          aInitState = AlphaConv { aId = 0, aEnv = M.empty, aTp = M.empty }

          getVar :: KId -> AlphaConvM KId
          getVar s = do
              st@(AlphaConv {aEnv = env, aTp = tps}) <- get

              let n = M.lookup s env
              let tp = M.lookup s tps

              trace ("TRACE: getVar " ++  s ++ " " ++ (show n) ++ " "  ++ (show tp)) $ return ()

              case (n, tp) of
                (Just nv, Just t) -> (lift $ addEntry False nv t) >> put st {aTp = M.delete nv tps}
                _                 -> return ()

              return $ maybe s id $ M.lookup s env

          replVar :: KId -> AlphaConvM KId
          replVar s = do
              st@(AlphaConv {aId = n, aEnv = env, aTp = tp}) <- get
              let nv = s ++ "_" ++ show n
              put st { aId = n+1, aEnv = M.insert s nv env }
              return nv
 
