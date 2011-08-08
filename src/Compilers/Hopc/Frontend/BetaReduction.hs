module Compilers.Hopc.Frontend.BetaReduction where

import Compilers.Hopc.Frontend.KTree

import Debug.Trace

import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Data.Generics.Biplate

type BStateM = State (M.Map KId KId)

betaReduce :: KTree -> KTree
betaReduce k = evalState (descendBiM tr k) M.empty
    where tr :: KTree -> BStateM KTree

          tr (KLet s e1 e2) = do
            e1'  <- tr e1 >>= addRepl s
            e2' <- tr e2
            return $ KLet s e1' e2'

          tr (KLetR binds e2) = do
            binds' <- forM binds $ \(s, e) -> do
                e' <- tr e >>= addRepl s
                return (s, e')
            e2' <- tr e2
            return $ KLetR binds' e2'

          tr (KVar s) = getV s >>= return . KVar

          tr (KApp fn args) = do
            fn'   <- getV fn
            args' <- mapM getV args
            return $ KApp fn' args'

          tr (KLambda args e) = do
            let nm  = [(bn, n) | KLet bn (KVar n) _ <- universe e]
            let nm2 = concat [bs | KLetR bs _ <- universe e]
            let nm3 = foldl kv nm nm2
            forM_ nm $ uncurry putV
            e' <- tr e
            return $ KLambda args e'

            where kv acc (bn, KVar n) | n `elem` args = acc ++ [(bn, n)]
                                      | otherwise = acc
                  kv acc x  = acc 

          tr x = return x

          putV x s = do
            env <- get
            put $ M.insert x s env

          getV s = do
            env <- get
            return $ maybe s id $ M.lookup s env

          addRepl s e@(KVar sn) = putV s sn >> return e

          addRepl s e = return e

