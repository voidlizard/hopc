{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.Opt where

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

optimize :: (I.Proc -> M a) -> I.Proc -> CompileM a
optimize f b = do
   return (runSimpleUniqueMonad $ runWithFuel 99999 (f b))


deadAssignElim :: I.Proc -> M I.Proc
deadAssignElim p@(I.Proc { I.entry = entry, I.body = g }) = do
    (g', _, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) g mapEmpty
    return p { I.body = g' }

    where bwd  = BwdPass { bp_lattice = liveLattice, bp_transfer = liveness
                          ,bp_rewrite = deadAsstElim }


