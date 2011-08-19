{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.FromVM where

import Compilers.Hopc.Id
import Compilers.Hopc.Compile
import qualified Compilers.Hopc.Backend.TinyC.VM as V
import Compilers.Hopc.Backend.TinyC.FromClosure
import qualified Compilers.Hopc.Backend.TinyC.IR as I

import Compilers.Hopc.Backend.TinyC.Live

import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad

import Compiler.Hoopl hiding (Block)

type LabelM = StateT (M.Map V.LabelId Label) I.M

convertIR :: V.LabelId -> [Block] -> CompileM I.Proc
convertIR n b = do
    (l, g) <- return (runSimpleUniqueMonad $ runWithFuel 99999 (body b))
    return $ I.Proc { I.name = n, I.entry = l, I.body = g} 

body :: [Block] -> I.M (Label, Graph I.Insn C C)
body bs@((Block {first = (Just (V.LABEL n))}):_) = do
    flip evalStateT M.empty $ do
        g <- foldl (liftM2 (|*><*|)) (return emptyClosedGraph) (map block bs)
        l <- labelFor n
        return $ (l, g)

body _ = error "COMPILER ERROR / INVALID CODE STRUCTURE / EMPTY FUNCTION" -- FIXME

block :: Block -> LabelM (Graph I.Insn C C)
block (Block (Just f) m (Just l) )  = do
    f' <- toFirst f
    m' <- mapM toMid m
    l' <- toLast l
    return $ mkFirst f' <*> mkMiddles m' <*> mkLast l'

block _  = error "COMPILER ERROR / INVALID CODE STRUCTURE I" -- FIXME

toFirst :: V.Op -> LabelM (I.Insn C O)
toFirst (V.LABEL n) =  labelFor n >>= (return.I.Label)

toFirst _ = error "COMPILER ERROR / INVALID CODE STRUCTURE III" -- FIXME

toMid :: V.Op -> LabelM (I.Insn O O)
toMid (V.CONST _ r) = return $ I.Const r
toMid (V.MOV r1 r2) = return $ I.Store r1 r2
toMid (V.MAKE_CLOSURE n rs) = return $ I.MClos n rs

toMid x = error $ "COMPILER ERROR / INVALID CODE STRUCTURE III " ++ (show x) -- FIXME

toLast :: V.Op -> LabelM (I.Insn O C)
toLast (V.JUMP l) = labelFor l >>= return . I.Branch
toLast (V.CJUMP(V.JumpFake r) l1 l2) = do
    l1' <- labelFor l1
    l2' <- labelFor l2
    return $ I.Cond r l1' l2'

toLast (V.CALL_FOREIGN n rs l) = do
    l' <- labelFor l
    return $ I.CallF n rs l'

toLast (V.CALL_LOCAL n rs l) = labelFor l >>= return . (I.CallL n rs)

toLast (V.CALL_CLOSURE r rs l) = labelFor l >>= return . (I.CallC r rs)

toLast (V.RET) = return I.Return

toLast _ = error "COMPILER ERROR / INVALID CODE STRUCTURE III" -- FIXME

labelFor :: V.LabelId -> LabelM Label
labelFor n = do
    l  <- gets (M.lookup n)
    case l of
        Nothing -> do l <- lift $ freshLabel
                      modify (M.insert n l)
                      return l

        Just x  -> return x

