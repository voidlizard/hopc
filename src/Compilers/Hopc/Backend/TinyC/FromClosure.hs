{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.FromClosure where

import Prelude hiding (last)

import Compilers.Hopc.Compile
import Compilers.Hopc.Id (KId)
import Compilers.Hopc.Frontend.Closure
import Compilers.Hopc.Frontend.Types
import Compilers.Hopc.Backend.TinyC.IR
import qualified Compilers.Hopc.Backend.TinyC.IR as I
import Compilers.Hopc.Backend.TinyC.Lit

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Data
import Data.Typeable
import Data.Generics.PlateData

import Text.Printf
import Debug.Trace

import Compiler.Hoopl

data C1 = C1 { tdict :: TDict }
type LabelM = StateT C1 M

convert :: Closure -> CompileM [Proc]
convert k = do
    e <- getEntries
    t <- nextTmp
    lift $ procs e k

    where

        procs :: TDict -> Closure -> M [I.Proc]
        procs e k = do
            let fns = [b | b@(CFun (Fun fn _ _ _)) <- universe k]
            mapM (proc e) fns

        proc :: TDict -> Closure -> M I.Proc 
        proc e (CFun (Fun n a f b)) = flip evalStateT (C1 e) $ do
            let retval = retvalVariable 
            entry  <- newLabel
            end    <- newLabel
            (b, g) <- tr retval (mkFirst (Label entry)) emptyClosedGraph b
            let b' = g |*><*| b <*> mkLast (I.Return retval)
            return $ Proc { name = n, args = a++f, freevarsnum=length f, entry = entry, body = b'}

        proc e x = error $ "NOT A FUNCTION: " ++ (show x) -- FIXME

        tr :: KId -> Graph Insn C O -> Graph Insn C C -> Closure -> LabelM (Graph Insn C O, Graph Insn C C)

        tr v b g (CInt i) = do
            let c = (Const (LInt i) v)
            let b' = b <*> mkMiddle c -- :: Graph Insn O O
            endblock (b', g)

        tr v b g (CStr s) = do
            let c = (Const (LStr s) v)
            let b' = b <*> mkMiddle c -- :: Graph Insn O O
            endblock (b', g)

        tr v b g (CVar n) = do
            tp <- gets tdict >>= return . M.lookup n
            withType tp $ do
                let c = (Assign v n)
                let b' = b <*> mkMiddle c -- :: Graph Insn O O
                endblock (b', g)
            where withType (Just TUnit) f = endblock (b,g)
                  withType _ f = f

        tr v b g (CUnit) = do
            return (b, g)

        tr var b g (CMakeCls n args) = do
            let b' = b <*> mkMiddle (MkClos n (maybe [] id args) var)
            endblock (b', g)

        tr var b g (CApplDir n args) = do
            ln <- newLabel 
            let b' = b <*> mkLast (Call ln (Direct n False) args var)
            let b'' = mkFirst (I.Label ln)
            return (b'', g |*><*| b')

        tr var b g (CApplCls n args) = do
            ln <- newLabel 
            let b' = b <*> mkLast (Call ln (Closure n False) args var)
            let b'' = mkFirst (I.Label ln)
            return (b'', g |*><*| b')

        tr v b g (CCond n c1 c2) = do
            l1 <- newLabel
            l2 <- newLabel
            l3 <- newLabel

            (b1, g')  <- tr v (mkFirst (I.Label l1)) g  c1 --- rename v unique --- move v' at exit from conditional v
            (b2, g'') <- tr v (mkFirst (I.Label l2)) g' c2
            
            let b3 = mkFirst (I.Label l3)

            let b1' = b1 <*> mkLast (I.Branch l3)
            let b2' = b2 <*> mkLast (I.Branch l3)
            let b'  = b  <*> mkLast (I.Cond n l1 l2)

            return (b3, g'' |*><*| b' |*><*| b1' |*><*| b2')

        tr v b g (CLet n c1 c2) = do
            (b', g') <- tr n b g c1
            tr v b' g' c2

        tr v b g (CLetR bs c2) = do
            (b', g') <- foldM f (b, g) bs
            tr v b' g' c2
            where f (b, g) (n, c1) = tr n b g c1

        tr var b g (CFun _) = error "UNEXPECTED CFUN"

        endblock (b, g) = do
            l <- newLabel
            return ( mkFirst (I.Label l), g |*><*| b <*> mkLast (  Branch l ) )

newLabel :: LabelM Label
newLabel = lift $ freshLabel

--labelFor :: V.LabelId -> LabelM Label
--labelFor n = do
--    l  <- gets (M.lookup n)
--    case l of
--        Nothing -> do l <- lift $ freshLabel
--                      modify (M.insert n l)
--                      return l

--        Just x  -> return x

