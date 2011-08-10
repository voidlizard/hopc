module Compilers.Hopc.Frontend.Eliminate where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Either
import Control.Monad.State
import Data.Data
import Data.Typeable
import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass

import Text.Printf
import Debug.Trace

import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Closure

data Elim = Elim { elenv :: S.Set KId } 
type ElimM = State Elim 

eliminate :: Closure -> Closure
eliminate k = trace "TRACE: eliminate" $
    evalState (descendBiM tr k) init
    where tr :: Closure -> ElimM Closure

          tr x@(CLet n e1 e2) = do
            e1' <- tr e1
            e2' <- tr e2
            let live = S.fromList $ usage e2
            if S.member n live || effect e1'
                then return $ CLet n e1' e2'
                else return e2'

          tr x@(CLetR binds e) = do
            e'     <- tr e
            binds' <- elim e [] (reverse binds) --mapM trB binds
            return $ CLetR binds' e'

          tr (CFun (Fun fn args free e)) = do
            e' <- tr e
            return $ CFun (Fun fn args free e')

          tr x = return x

          trB (n, e)  = do
            e' <- tr e
            return (n, e')

          flt live (n, (CFun _)) = S.member n live
          flt live (n, e) = S.member n live || effect e

          usage x = para u x
          u (CApplCls n args) r = concat r ++ n:args
          u (CApplDir n args) r = concat r ++ n:args
          u (CVar n) r = concat r ++ [n]
          u (CMakeCls n args) r = concat r ++ n:args
          u x r = concat r

          init = Elim S.empty

          elim e l (r:rs) = do
            let ebs = e : map snd l
            let live = S.fromList $ concat $ map usage ebs
            r' <- trB r
            if flt live r'
              then elim e (r':l) rs
              else elim e l rs

          elim e l [] = return l

effect :: Closure -> Bool
effect k = foldl (||) False $ para eff k
    where eff (CVar n) r = False : concat r
          eff (CMakeCls n args) r = False : concat r
          eff (CFun (Fun n _ _ e)) r = False : concat r
          eff x r = True : concat r

