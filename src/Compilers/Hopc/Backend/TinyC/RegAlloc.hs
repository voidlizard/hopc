{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.RegAlloc where

import Compilers.Hopc.Id
import Compilers.Hopc.Compile
import qualified Compilers.Hopc.Backend.TinyC.VM as V
import Compilers.Hopc.Backend.TinyC.VM (R) 
import Compilers.Hopc.Backend.TinyC.FromClosure
import qualified Compilers.Hopc.Backend.TinyC.IR as I

import Compilers.Hopc.Backend.TinyC.Live

import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad

import Compiler.Hoopl hiding (Block)

data RegEnvFact = RegEnvFact { liveRegs :: [R]
                              ,freeRegs :: [R]
                              ,spilledRegs :: M.Map R Int
                              ,slot :: Int
                              ,freeSlots :: [Int]
                             }

regAlloc :: I.Proc -> CompileM I.Proc
regAlloc b = do
   return (runSimpleUniqueMonad $ runWithFuel 99999 (linearScan b))

linearScan :: I.Proc -> I.M I.Proc
linearScan p@(I.Proc { I.entry = entry, I.body = g }) = do
    undefined
--    (g', _, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) g mapEmpty
--    return p { I.body = g' }

--    where bwd  = BwdPass { bp_lattice = liveLattice, bp_transfer = liveness
--                          ,bp_rewrite = deadAsstElim }


