{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.CWriter where

import Compilers.Hopc.Compile
import Compilers.Hopc.Backend.TinyC.VM

import qualified Compiler.Hoopl as H
import Control.Monad.Writer
import Control.Monad.State

write :: [Proc] -> CompileM [String]
write p = runCWriterM $ do
    prologue
    entrypoint $ forM_ p $ \p -> do
      forM_ (body p) opcode
    epilogue

  where
    prologue :: CWriterM ()
    prologue = do
      indent "#include <hopcruntime.h>"
      empty

    epilogue :: CWriterM ()
    epilogue = nothing

    entrypoint :: CWriterM () -> CWriterM () 
    entrypoint m = do 
      indent "void hopc_entrypoint() {"
      gotoLabel "entrypoint"
      pushIndent
      indent "switch(R0) {"
      pushIndent
      m
      popIndent
      indent "}"
      popIndent
      indent "}"

    gotoLabel :: String -> CWriterM ()
    gotoLabel s = indent $ s ++ ":"

    indent :: String -> CWriterM ()
    indent s = do
      i <- get
      let shift = concat $ replicate i "    "
      tell [shift ++ s]

    empty = tell [""]

    nothing = return ()

    caseLabel :: H.Label -> CWriterM ()
    caseLabel n = indent $ "case " ++ show n ++ ":"

    opcode :: Op -> CWriterM ()
    opcode (Label n) = gotoLabel (show n) >> caseLabel n
    opcode _ = nothing 

    pushIndent :: CWriterM ()
    pushIndent = modify succ

    popIndent :: CWriterM ()
    popIndent = modify pred


type CWriterM = StateT Int (WriterT [String] CompileM)

runCWriterM :: CWriterM a -> CompileM [String]
runCWriterM = execWriterT . flip evalStateT 0 

