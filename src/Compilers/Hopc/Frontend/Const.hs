module Compilers.Hopc.Frontend.Const where

import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Compile

import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State
import Data.Generics.Biplate

type ConstPropM = StateT (M.Map KId KTree)  CompileM

-- Just a dummy

propagate :: KTree -> CompileM KTree
propagate k = do
    k' <- evalStateT (descendBiM tr k) init
    return k'

    where tr :: KTree -> ConstPropM KTree

          tr (KLet n b e) = do
            (n', b') <- trB (n, b)
            e' <- tr e
            return $ KLet n' b'  e'

          tr (KLetR b e) = do
            forM_ b trB
            b' <- forM b trB
            e' <- tr e
            return $ KLetR b' e'

          tr (KLambda args e) = do
            e' <- tr e
            return $ KLambda args e'

          tr c@(KVar n) = do
            e <- repl c n
            return e

          tr k = return k

          trB (n, e@(KInt v)) = do
            mem n e
            return (n, e)

          trB (n, e@(KStr v)) = do
            mem n e
            return (n, e)

          trB (n, e) = do
            e' <- tr e
            return (n, e')

          mem n v = modify (M.insert n v)

          repl :: KTree -> KId -> ConstPropM KTree
          repl e n = do
            v <- gets (M.lookup n)
            if isJust v then (return.fromJust) v else return e

          init = M.empty 

