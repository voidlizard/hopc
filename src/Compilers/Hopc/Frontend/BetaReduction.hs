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
            e1'  <- tr e1
            e1'' <- case e1' of
                        KVar sn -> putV s sn >> return e1'
                        e       -> return e

            e2' <- tr e2
            return $ KLet s e1'' e2'

          tr (KVar s) = getV s >>= return . KVar

          tr (KApp fn args) = do
            fn'   <- getV fn
            args' <- mapM getV args
            return $ KApp fn' args'

          tr x = return x

          putV x s = do
            env <- get
            put $ M.insert x s env

          getV s = do
            env <- get
            return $ maybe s id $ M.lookup s env

