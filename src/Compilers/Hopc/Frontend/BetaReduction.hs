module Compilers.Hopc.Frontend.BetaReduction where

import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Compile

import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
import Control.Monad.Trans
import Data.Generics.Biplate

import Debug.Trace
import Text.Printf

betaReduceM :: KTree -> CompileM KTree
betaReduceM = return . betaReduce

betaReduce :: KTree -> KTree
betaReduce k = evalState (evalStateT (descendBiM tr k) M.empty) S.empty
    where 

      tr (KLet s e1 e2) = do
        notice s
        e1'  <- tr e1 >>= addRepl s
        e2' <- tr e2
        return $ KLet s e1' e2'

      tr (KLetR binds e2) = do
        binds' <- forM binds $ \(s, e) -> do
            notice s
            e' <- tr e >>= addRepl s
            return (s, e')
        e2' <- tr e2
        return $ KLetR binds' e2'

      tr (KVar s) = getV s >>= return . KVar

      tr (KApp fn args) = do
        fn'   <- getV fn
        args' <- mapM getV args
        return $ KApp fn' args'

      tr (KCond t e1 e2) = do
        t'   <- getV t
        e1' <- tr e1
        e2' <- tr e2
        return $ KCond t' e1' e2'

      tr (KLambda args e) = do
        mapM_ notice args
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
        env  <- get
        let repl = maybe s id $ M.lookup s env
        known <- noticed repl
        if known
          then return repl
          else return s

      addRepl s e@(KVar sn) = putV s sn >> return e

      addRepl s e = return e

      notice s = lift $ modify (S.insert s)

      noticed s = lift $ gets (S.member s)


